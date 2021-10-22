use proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let mut builder_inners = quote!();
    let mut builder_constructor_inners = quote!();
    let ident = ast.ident;
    let builder_ident = format_ident!("{}Builder", ident);
    let data_struct = match ast.data {
        syn::Data::Struct(data_struct) => data_struct,
        _ => unimplemented!(),
    };
    match data_struct.fields {
        syn::Fields::Named(fields) => {
            for field in fields.named.iter() {
                let ty = &field.ty;
                let ident = field.ident.as_ref().unwrap();
                builder_inners.extend(quote! { #ident: Option<#ty>, });
                builder_constructor_inners.extend(quote! { #ident: None, });
            }
        }
        _=> unimplemented!(),
    };

    let expanded = quote! {
        pub struct #builder_ident {
            #builder_inners
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #builder_constructor_inners
                }
            }
        }
    };

    TokenStream::from(expanded)
}
