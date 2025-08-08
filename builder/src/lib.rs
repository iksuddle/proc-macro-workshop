use proc_macro::TokenStream;
use quote::format_ident;
use syn::parse::Parse;
use syn::parse_macro_input;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    let name = input.ident;
    let builder_ident = format_ident!("{}Builder", name);

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(fields),
        ..
    }) = input.data
    {
        fields.named
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

    let builder_vec_setters = fields.iter().map(|f| {
        let builder_attr = get_attr(f, "builder")?;
        let name = get_meta_list_value(builder_attr, "each")?;

        let vec_name = &f.ident;
        let ty = get_inner_ty("Vec", &f.ty);
        Some(quote::quote! {
            pub fn #name (&mut self, #name: #ty) -> &mut Self{
                match &mut self.#vec_name {
                    Some(v) => v.push(#name),
                    None => self.#vec_name = Some(vec![#name]),
                }
                self
            }
        })
    });

    let builder_setters = fields.iter().map(|f| {
        let builder_attr = get_attr(f, "builder");
        if let Some(attr) = builder_attr {
            let name = get_meta_list_value(attr, "each");
            if f.ident == name {
                return None;
            }
        }

        let name = &f.ident;
        let mut ty = &f.ty;

        if let Some(t) = get_inner_ty("Option", ty) {
            ty = t;
        }

        Some(quote::quote! {
            pub fn #name (&mut self, #name: #ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        })
    });

    let built_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let mut field = quote::quote! {
            #name: self.#name.clone().unwrap_or_default(),
        };

        if get_inner_ty("Option", &f.ty).is_some() {
            field = quote::quote! {
                #name: self.#name.clone(),
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

            #(#builder_vec_setters)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#built_fields)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}

fn get_attr<'a>(f: &'a syn::Field, ident: &str) -> Option<&'a syn::Attribute> {
    for attr in &f.attrs {
        if let syn::Meta::List(meta_list) = &attr.meta {
            if meta_list.path.is_ident(ident) {
                return Some(attr);
            }
        }
    }

    None
}

fn get_meta_list_value(attr: &syn::Attribute, inner_path_ident: &str) -> Option<syn::Ident> {
    if let syn::Meta::List(_) = &attr.meta {
        let expr_assign = attr
            .parse_args_with(syn::ExprAssign::parse)
            .expect("error parsing attr");

        match *expr_assign.left {
            syn::Expr::Path(expr_path) => {
                if !expr_path.path.is_ident(inner_path_ident) {
                    return None;
                }
            }
            _ => unimplemented!(),
        }

        let name = match *expr_assign.right {
            syn::Expr::Lit(expr_lit) => match &expr_lit.lit {
                syn::Lit::Str(lit_str) => lit_str.value(),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        let name = quote::format_ident!("{}", name);
        return Some(name);
    }

    None
}

fn get_inner_ty<'a>(wrapper: &'a str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(type_path) = ty {
        // todo: figure out why this is not the same as !path.is_ident(wrapper)
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
