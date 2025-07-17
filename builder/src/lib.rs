use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // parse input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    let vis = &input.vis;
    let name = &input.ident;
    let builder_name = format!("{}Builder", &input.ident);
    let builder_ident = syn::Ident::new(&builder_name, name.span());

    let struct_data = match &input.data {
        syn::Data::Struct(data_struct) => data_struct,
        _ => unimplemented!(),
    };

    let fields = match &struct_data.fields {
        Fields::Named(fields) => fields,
        _ => unimplemented!(),
    };

    let struct_fields = fields.named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            #name: Option<#ty>,
        }
    });

    let init_fields = fields.named.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: None,
        }
    });

    let setters = fields.named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            pub fn #name (&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let built_fields = fields.named.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: self.#name.clone().ok_or("not built!".to_owned())?,
        }
    });

    let expanded = quote! {
        #vis struct #builder_ident {
            #(#struct_fields)*
        }

        impl #name {
            #vis fn builder() -> #builder_ident {
                #builder_ident {
                    #(#init_fields)*
                }
            }
        }

        impl #builder_ident {
            #(#setters)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#built_fields)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
