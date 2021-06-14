use proc_macro::TokenStream;
use quote::{quote, ToTokens};

struct DebugField<T: ToTokens> {
    named: T,
    format: Option<String>,
}

fn get_associated_type<'a>(t: &'a syn::Type, generics: &syn::Generics) -> Option<&'a syn::Path> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = t {
        let segments = &path.segments;
        if segments.len() > 1
            && generics
                .type_params()
                .any(|gen| gen.ident == segments[0].ident)
        {
            return Some(path);
        }
        for segment in segments {
            let syn::PathSegment { arguments, .. } = segment;
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) = arguments
            {
                for arg in args {
                    if let syn::GenericArgument::Type(t) = arg {
                        return get_associated_type(t, generics);
                    }
                }
            };
        }
    };
    None
}

fn get_ultimate_identifier(t: &syn::Type) -> &syn::Ident {
    if let syn::Type::Path(syn::TypePath { path, .. }) = t {
        for segment in &path.segments {
            let syn::PathSegment { arguments, .. } = segment;
            match arguments {
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) => {
                    for arg in args {
                        if let syn::GenericArgument::Type(t) = arg {
                            return get_ultimate_identifier(t);
                        };
                    }
                }
                syn::PathArguments::None => return &segment.ident,
                _ => (),
            };
        }
    };
    panic!("No identifier!")
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);

    let structname = parsed_input.ident;
    let mut generics = parsed_input.generics;
    let mut associates_types = Vec::new();

    let attr_bounds: Vec<_> = parsed_input
        .attrs
        .iter()
        .filter_map(|attr| {
            if let Ok(syn::Meta::List(syn::MetaList { path, nested, .. })) = attr.parse_meta() {
                if path.is_ident("debug") {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                        path: inner_path,
                        lit: syn::Lit::Str(litstr),
                        ..
                    }))) = nested.first()
                    {
                        if inner_path.is_ident("bound") {
                            if let Ok(where_predicate) =
                                syn::parse_str::<syn::WherePredicate>(&litstr.value())
                            {
                                return Some(where_predicate);
                            }
                        }
                    }
                }
            }
            None
        })
        .collect();

    let named_fields = match parsed_input.data {
        syn::Data::Struct(
            syn::DataStruct {
                fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
                ..
            },
            ..,
        ) => named
            .iter()
            .map(|field| {
                if let syn::Type::Path(syn::TypePath { path, .. }) = &field.ty {
                    if let Some(at) = get_associated_type(&field.ty, &generics) {
                        associates_types.push(at);
                    }

                    for segment in &path.segments {
                        if segment.ident != "PhantomData" {
                            let ident = get_ultimate_identifier(&field.ty);
                            for generic_param in generics.params.iter_mut() {
                                if let syn::GenericParam::Type(generic_tp) = generic_param {
                                    if &generic_tp.ident == ident
                                        && !associates_types
                                            .iter()
                                            .map(|&at| &at.segments[0].ident)
                                            .any(|at_ident| at_ident == ident)
                                        && !attr_bounds.iter().any(|bound| {
                                            if let syn::WherePredicate::Type(syn::PredicateType {
                                                bounded_ty: syn::Type::Path(tp),
                                                ..
                                            }) = &bound
                                            {
                                                if let Some(ident) = tp
                                                    .path
                                                    .segments
                                                    .first()
                                                    .map(|segment| &segment.ident)
                                                {
                                                    return &generic_tp.ident == ident;
                                                }
                                            }
                                            false
                                        })
                                    {
                                        generic_tp
                                            .bounds
                                            .push(syn::parse_quote!(::std::fmt::Debug));
                                    }
                                }
                            }
                        }
                    }
                };
                let syn::Field { attrs, .. } = field;

                if let Some(attr) = attrs.first() {
                    let syn::Attribute { path, .. } = &attr;
                    if path.is_ident("debug") {
                        if let Ok(syn::Meta::NameValue(syn::MetaNameValue {
                            lit: syn::Lit::Str(litstr),
                            ..
                        })) = attr.parse_meta()
                        {
                            return DebugField {
                                named: field,
                                format: Some(litstr.value()),
                            };
                        }
                    }
                }
                DebugField {
                    named: field,
                    format: None,
                }
            })
            .collect::<Vec<_>>(),
        _ => panic!("no fields!"),
    };

    let where_clause = generics.make_where_clause();
    for at in associates_types {
        where_clause
            .predicates
            .push(syn::parse_quote!(#at : ::std::fmt::Debug));
    }
    for bound in attr_bounds {
        where_clause.predicates.push(bound);
    }

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let output_fields = named_fields
        .into_iter()
        .map(|DebugField { named, format }| {
            let ident = &named.ident;
            if let Some(ref format) = format {
                quote! { .field(stringify!(#ident), &format_args!(#format, &self.#ident)) }
            } else {
                quote! { .field(stringify!(#ident), &self.#ident) }
            }
        })
        .collect::<Vec<_>>();

    let output = quote! {
        impl #impl_generics ::std::fmt::Debug for #structname #type_generics #where_clause
        {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result  {
                fmt.debug_struct(stringify!(#structname))
                    #(#output_fields)*
                    .finish()
            }
        }
    };
    output.into()
}
