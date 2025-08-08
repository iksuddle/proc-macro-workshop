use proc_macro::TokenStream;
use quote::format_ident;
use syn::{parse::Parse, parse_macro_input, ExprAssign};

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
        let name = get_meta_list_value(&f, "builder", "each");
        if name.is_none() {
            return None;
        }

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
        let name = get_meta_list_value(&f, "builder", "each");
        if f.ident == name {
            return None;
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

fn get_meta_list_value(
    f: &syn::Field,
    meta_list_path_ident: &str,
    inner_path_ident: &str,
) -> Option<syn::Ident> {
    for attr in &f.attrs {
        match &attr.meta {
            syn::Meta::List(meta_list) => {
                // ignore attributes that aren't like #[builder(...)]
                if !meta_list.path.is_ident(meta_list_path_ident) {
                    continue;
                }
                let p = attr
                    .parse_args_with(ExprAssign::parse)
                    .expect("error parsing attribute");

                let left = p.left;

                match &*left {
                    syn::Expr::Path(expr_path) => {
                        // ignore attributes that aren't like #[builder(each = "...")]
                        if !expr_path.path.is_ident(inner_path_ident) {
                            continue;
                        }
                    }
                    _ => unimplemented!(),
                };

                // extract the string literal
                let right = p.right;
                let name = match &*right {
                    syn::Expr::Lit(expr_lit) => match &expr_lit.lit {
                        syn::Lit::Str(lit_str) => lit_str.value(),
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                };

                let name = quote::format_ident!("{}", name);
                return Some(name);
            }
            _ => unimplemented!(),
        }
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
