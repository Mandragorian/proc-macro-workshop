use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span};
use quote::{format_ident, quote, quote_spanned};
use syn;
use syn::spanned::Spanned;

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

enum BuilderError {
    Error(String, Span),
}

fn get_field_builder_type<'a>(
    optional_segment: Option<&'a syn::PathSegment>,
    attrs: &'a Vec<syn::Attribute>,
    ident: &'a syn::Ident,
) -> Result<FieldBuilderType<'a>, BuilderError> {
    let filtered_attrs = attrs.iter()
        .filter_map(|attr| attr.parse_meta().ok() );
        //.find_map(|meta: syn::Meta| {
    let mut our_attrs: Option<String> = None;
    for meta in filtered_attrs {
            match &meta {
                syn::Meta::List(meta_list) => {
                    if meta_list.path.is_ident("builder") {
                        let f = meta_list.nested.first().unwrap();
                        if let syn::NestedMeta::Meta(v) = f {
                            if let syn::Meta::NameValue(name_value) = v {
                                if name_value.path.is_ident("each") {
                                    if let syn::Lit::Str(lit) = &name_value.lit {
                                        our_attrs = Some(lit.value());
                                    } else {
                                        panic!("expected string literal");
                                    }
                                } else {
                                    let error_msg = format!("expected `builder(each = \"...\")`");
                                    return Err(BuilderError::Error(error_msg, meta.span()));
                                }
                            }
                        }
                    }
                }
                _ => (),
            }
        };

    if let Some(segment) = optional_segment {
        match segment.ident.to_string().as_str() {
            "Option" => {
                let argument_opt = get_optional_argument_from_type_arguments(segment);
                Ok(FieldBuilderType::Optional(argument_opt.unwrap()))
            },
            "Vec" => {
                let argument_opt = get_optional_argument_from_type_arguments(segment);
                if let Some(each_func) = our_attrs {
                    if ident != each_func.as_str() {
                        Ok(FieldBuilderType::Vectoral(argument_opt.unwrap(), each_func))
                    } else {
                        Ok(FieldBuilderType::VectoralNoSetAll(argument_opt.unwrap(), each_func))
                    }
                } else {
                    Ok(FieldBuilderType::Simple)
                }
            }
            _ => {
                Ok(FieldBuilderType::Simple)
            }
        }
    } else {
        Ok(FieldBuilderType::Simple)
    }
}

enum FieldBuilderType<'a> {
    Simple,
    Optional(&'a syn::Type),
    Vectoral(&'a syn::Type, String),
    VectoralNoSetAll(&'a syn::Type, String),
}

fn determine_field_type<'a>(ty: &'a syn::Type, attrs: &'a Vec<syn::Attribute>, field_ident: &'a syn::Ident) -> Result<FieldBuilderType<'a>, BuilderError> {
    if let syn::Type::Path(tpath) = ty {
        let mut iter = tpath.path.segments.iter();
        let must_be_option = iter.next();
        let must_be_none = iter.next();

        let res = get_field_builder_type(must_be_option, attrs, field_ident);

        match res? {
            FieldBuilderType::Simple => Ok(FieldBuilderType::Simple),
            FieldBuilderType::Optional(inner) => {
                if must_be_none.is_some() {
                    panic!("wrong type parameters for Option")
                }
                Ok(FieldBuilderType::Optional(inner))
            }
            FieldBuilderType::Vectoral(inner, each_func) => {
                if must_be_none.is_some() {
                    panic!("wrong type parameters for Vec")
                }
                Ok(FieldBuilderType::Vectoral(inner, each_func))
            }
            FieldBuilderType::VectoralNoSetAll(inner, each_func) => {
                if must_be_none.is_some() {
                    panic!("wrong type parameters for Vec")
                }
                Ok(FieldBuilderType::VectoralNoSetAll(inner, each_func))
            }
        }
    } else {
        panic!("type wasn't path");
    }
}

fn handle_field(
    field: &syn::Field,
    builder: &mut BuilderBuilder,
) {
    let ty = &field.ty;
    let attrs = &field.attrs;
    let ident = field.ident.as_ref().unwrap();
    //let field_builder_type = determine_field_type(ty, attrs, ident);

    match determine_field_type(ty, attrs, ident) {
        Ok(field_builder_type) =>
            match field_builder_type {
            FieldBuilderType::Optional(inner) =>
                builder.add_optional(ident, inner),
            FieldBuilderType::Simple =>
                builder.add_simple(ident, ty),
            FieldBuilderType::Vectoral(inner, each_func) =>
                builder.add_vectoral(ident, inner, each_func),
            FieldBuilderType::VectoralNoSetAll(inner, each_func) =>
                builder.add_vectoral_no_set_all(ident, inner, each_func),
        }
        Err(err) => builder.add_error(err),
    }
}

struct BuilderBuilder {
    builder_inners: TokenStream2,
    builder_constructor_inners: TokenStream2,
    builder_methods: TokenStream2,
    builder_checks: TokenStream2,
    target_construction: TokenStream2,
    errors: TokenStream2,
    ident: syn::Ident,
    has_errors: bool,
}

impl BuilderBuilder {
    fn new(ident: syn::Ident) -> Self {
        let builder_inners = quote!();
        let builder_constructor_inners = quote!();
        let builder_methods = quote!();
        let builder_checks = quote!();
        let target_construction = quote!();
        let errors = quote!();

        BuilderBuilder {
            builder_inners,
            builder_constructor_inners,
            builder_methods,
            builder_checks,
            target_construction,
            ident,
            errors,
            has_errors: false,
        }
    }

    fn add_error(&mut self, error: BuilderError) {
        self.has_errors = true;
        match error {
            BuilderError::Error(msg, span) => {
                self.errors.extend(quote_spanned! {
                    span => compile_error!(#msg);
                });
            }
        }
    }

    fn add_vectoral(&mut self, ident: &syn::Ident, ty: &syn::Type, each: String) {
        let each_ident = format_ident!("{}", each);
        self.builder_inners.extend(quote! { #ident: Vec<#ty>, });

        self.builder_methods.extend(quote! {
            pub fn #ident(&mut self, #ident: Vec<#ty>) -> &mut Self {
                self.#ident = #ident;
                self
            }

            pub fn #each_ident(&mut self, #each_ident: #ty) -> &mut Self {
                self.#ident.push(#each_ident);
                self
            }
        });

        self.target_construction.extend(quote! {
            #ident: self.#ident.clone(),
        });

        self.add_constructor_inner_vec(ident);
    }

    fn add_vectoral_no_set_all(&mut self, ident: &syn::Ident, ty: &syn::Type, each: String) {
        let each_ident = format_ident!("{}", each);
        self.builder_inners.extend(quote! { #ident: Vec<#ty>, });

        self.builder_methods.extend(quote! {
            pub fn #each_ident(&mut self, #each_ident: #ty) -> &mut Self {
                self.#ident.push(#each_ident);
                self
            }
        });

        self.target_construction.extend(quote! {
            #ident: self.#ident.clone(),
        });

        self.add_constructor_inner_vec(ident);
    }

    fn add_simple(&mut self, ident: &syn::Ident, ty: &syn::Type) {
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

    fn add_constructor_inner_vec(&mut self, ident: &syn::Ident) {
        self.builder_constructor_inners.extend(quote! { #ident: Vec::new(), });
    }

    fn build(&self) -> TokenStream2 {
        let errors = &self.errors;
        let builder_inners = &self.builder_inners;
        let builder_constructor_inners = &self.builder_constructor_inners;
        let builder_methods = &self.builder_methods;
        let builder_checks = &self.builder_checks;
        let target_construction = &self.target_construction;
        let ident = &self.ident;
        let builder_ident = format_ident!("{}Builder", ident);

        if self.has_errors {
            return quote! {
                #errors
            };
        }

        quote! {
            #errors
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

#[proc_macro_derive(Builder, attributes(builder))]
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
