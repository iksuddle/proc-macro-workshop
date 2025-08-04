use proc_macro::TokenStream;
use quote::format_ident;
use syn::parse_macro_input;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    let name = input.ident;
    let builder_ident = format_ident!("{}Builder", name);

    let fields = if let syn::Data::Struct(data_struct) = input.data {
        if let syn::Fields::Named(fields) = data_struct.fields {
            fields.named
        } else {
            unimplemented!()
        }
    } else {
        unimplemented!()
    };

    let option_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let mut ty = &f.ty;

        if let Some(t) = get_inner_ty("Option", ty) {
            ty = t;
        }

        quote::quote! {
            #name: std::option::Option<#ty>,
        }
    });

    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;

        quote::quote! {
            #name: std::option::Option::None,
        }
    });

    let builder_setters = fields.iter().map(|f| {
        let name = &f.ident;
        let mut ty = &f.ty;

        if let Some(t) = get_inner_ty("Option", ty) {
            ty = t;
        }

        quote::quote! {
            fn #name (&mut self, #name: #ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        }
    });

    let built_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let mut field = quote::quote! {
            #name: self.#name.clone().ok_or("field not set")?,
        };

        if let Some(_) = get_inner_ty("Option", &f.ty) {
            field = quote::quote! {
                #name: self.#name.clone().or(None),
            };
        }

        field
    });

    let expanded = quote::quote! {
        pub struct #builder_ident {
            #(#option_fields)*
        }

        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_fields)*
                }
            }
        }

        impl #builder_ident {
            #(#builder_setters)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#built_fields)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}

fn get_inner_ty<'a>(wrapper: &'a str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(type_path) = ty {
        if type_path.path.segments.len() != 1 || type_path.path.segments[0].ident != wrapper {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref generic_args) =
            type_path.path.segments[0].arguments
        {
            if generic_args.args.len() != 1 {
                return None;
            }

            let arg = generic_args.args.first().unwrap();
            if let syn::GenericArgument::Type(ref inner_ty) = arg {
                return Some(inner_ty);
            }
        }
    }
    None
}
