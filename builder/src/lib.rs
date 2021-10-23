use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn;

fn get_data_struct_from_ast_data(ast_data: syn::Data) -> syn::DataStruct {
    match ast_data {
        syn::Data::Struct(data_struct) => data_struct,
        _ => panic!("Builder can only be derived for structs"),
    }
}

fn get_optional_argument_from_type_arguments(segment: &syn::PathSegment) -> Option<&syn::Type> {
    match &segment.arguments {
        syn::PathArguments::AngleBracketed(args) => {
            let mut args_iter = args.args.iter();
            args_iter
                .next()
                .map(|generic_arg| match generic_arg {
                    syn::GenericArgument::Type(ty) => Some(ty),
                    _ => None,
                })
                .flatten()
        }
        _ => panic!("non angle bracketed args in type"),
    }
}

fn get_option_arg_if_optional(
    optional_segment: Option<&syn::PathSegment>,
) -> (bool, Option<&syn::Type>) {
    if let Some(segment) = optional_segment {
        let is_option = segment.ident == "Option";

        let argument_opt = if is_option {
            get_optional_argument_from_type_arguments(segment)
        } else {
            None
        };
        (is_option, argument_opt)
    } else {
        (false, None)
    }
}

fn determine_if_field_is_optional(ty: &syn::Type) -> (bool, Option<&syn::Type>) {
    if let syn::Type::Path(tpath) = ty {
        let mut iter = tpath.path.segments.iter();
        let must_be_option = iter.next();
        let must_be_none = iter.next();

        let (start_with_option, argument) = get_option_arg_if_optional(must_be_option);

        (start_with_option && must_be_none.is_none(), argument)
    } else {
        panic!("type wasn't path");
    }
}

fn handle_field(
    field: &syn::Field,
    builder: &mut BuilderBuilder,
) {
    let ty = &field.ty;
    let (is_optional, inner) = determine_if_field_is_optional(ty);

    let ident = field.ident.as_ref().unwrap();
    if is_optional {
        builder.add_optional(ident, inner.unwrap());
    } else {
        builder.add_non_optional(ident, ty);
    }
}

struct BuilderBuilder {
    builder_inners: TokenStream2,
    builder_constructor_inners: TokenStream2,
    builder_methods: TokenStream2,
    builder_checks: TokenStream2,
    target_construction: TokenStream2,
    ident: syn::Ident,
}

impl BuilderBuilder {
    fn new(ident: syn::Ident) -> Self {
        let builder_inners = quote!();
        let builder_constructor_inners = quote!();
        let builder_methods = quote!();
        let builder_checks = quote!();
        let target_construction = quote!();

        BuilderBuilder {
            builder_inners,
            builder_constructor_inners,
            builder_methods,
            builder_checks,
            target_construction,
            ident,
        }
    }

    fn add_non_optional(&mut self, ident: &syn::Ident, ty: &syn::Type) {
        self.builder_inners.extend(quote! { #ident: Option<#ty>, });

        self.builder_methods.extend(quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        });

        self.builder_checks.extend(quote! {
            if self.#ident.is_none() {
                return Err("no fields can be None");
            }
        });

        self.target_construction.extend(quote! {
            #ident: self.#ident.clone().unwrap(),
        });

        self.add_constructor_inner(ident);
    }

    fn add_optional(&mut self, ident: &syn::Ident, ty: &syn::Type) {
        self.builder_inners.extend(quote! { #ident: Option<#ty>, });

        self.builder_methods.extend(quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        });

        self.target_construction.extend(quote! {
            #ident: self.#ident.clone(),
        });

        self.add_constructor_inner(ident);
    }

    fn add_constructor_inner(&mut self, ident: &syn::Ident) {
        self.builder_constructor_inners.extend(quote! { #ident: None, });
    }

    fn build(&self) -> TokenStream2 {
        let builder_inners = &self.builder_inners;
        let builder_constructor_inners = &self.builder_constructor_inners;
        let builder_methods = &self.builder_methods;
        let builder_checks = &self.builder_checks;
        let target_construction = &self.target_construction;
        let ident = &self.ident;
        let builder_ident = format_ident!("{}Builder", ident);

        quote! {
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
        }
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let ident = ast.ident;
    let mut builder = BuilderBuilder::new(ident);

    let data_struct = get_data_struct_from_ast_data(ast.data);

    match data_struct.fields {
        syn::Fields::Named(fields) => {
            for field in fields.named.iter() {
                handle_field(
                    field,
                    &mut builder,
                );
            }
        }
        _ => unimplemented!(),
    };

    let expanded = builder.build();

    TokenStream::from(expanded)
}
