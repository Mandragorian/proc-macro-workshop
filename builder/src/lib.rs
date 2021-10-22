use proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let mut builder_inners = quote!();
    let mut builder_constructor_inners = quote!();
    let mut builder_methods = quote!();
    let mut builder_checks = quote!();
    let mut target_construction = quote!();

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
                let (is_optional, inner) = if let syn::Type::Path(tpath) = ty {
                    let mut iter = tpath.path.segments.iter();
                    let must_be_option = iter.next();
                    let must_be_none = iter.next();

                    let (start_with_option, argument) = if let Some(segment) = must_be_option {
                        let is_option = segment.ident == "Option";

                        let argument_opt = if is_option {
                            match &segment.arguments {
                                syn::PathArguments::AngleBracketed(args) => {
                                    let mut args_iter = args.args.iter();
                                    args_iter.next().map(|generic_arg| {
                                        match generic_arg {
                                            syn::GenericArgument::Type(ty) => Some(ty),
                                            _ => None,
                                        }
                                    }).flatten()
                                }
                                _ => panic!("non angle bracketed args in type"),
                            }
                        } else {
                            None
                        };
                        (is_option, argument_opt)
                    } else {
                        (false, None)
                    };

                    (start_with_option && must_be_none.is_none(), argument)
                } else {
                    panic!("type wasn't path");
                };

                let ident = field.ident.as_ref().unwrap();
                if is_optional {
                    builder_inners.extend(quote! { #ident: #ty, });
                } else {
                    builder_inners.extend(quote! { #ident: Option<#ty>, });
                }
                builder_constructor_inners.extend(quote! { #ident: None, });

                if is_optional {
                    builder_methods.extend(quote! {
                        pub fn #ident(&mut self, #ident: #inner) -> &mut Self {
                            self.#ident = Some(#ident);
                            self
                        }
                    });
                } else {
                    builder_methods.extend(quote! {
                        pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = Some(#ident);
                            self
                        }
                    });
                }

                if !is_optional {
                    builder_checks.extend(quote! {
                        if self.#ident.is_none() {
                            return Err("no fields can be None");
                        }
                    });
                }

                if !is_optional {
                    target_construction.extend(quote! {
                        #ident: self.#ident.clone().unwrap(),
                    });
                } else {
                    target_construction.extend(quote! {
                        #ident: self.#ident.clone(),
                    });
                }
            }
        }
        _=> unimplemented!(),
    };

    let expanded = quote! {
        pub struct #builder_ident {
            #builder_inners
        }

        impl #builder_ident {
            #builder_methods

            pub fn build(&mut self) -> Result<#ident, &'static str> {
                #builder_checks

                Ok(Command {
                    #target_construction
                })
            }
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
