use proc_macro::TokenStream;
use quote::{quote, ToTokens};

struct DebugField<T: ToTokens> {
    named: T,
    format: Option<String>,
}

fn get_ultimate_identifier(t: &syn::Type) -> &syn::Ident {
    match t {
        syn::Type::Path(syn::TypePath{path: syn::Path{segments, ..}, ..}) => {
            for segment in segments {
                match segment {
                    syn::PathSegment{arguments, ..} => {
                        match arguments {
                            syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{args, ..}) => {
                                for arg in args {
                                    match arg {
                                        syn::GenericArgument::Type(t) => return get_ultimate_identifier(t),
                                        _ => (),
                                    }
                                };
                            },
                            syn::PathArguments::None => return &segment.ident,
                            _ => (),
                        };
                    }
                }
            }
        },
        _ => unimplemented!("No identifier here!"),
    }
    unimplemented!("No identifier here either!")
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);

    let structname = parsed_input.ident;
    let mut generics = parsed_input.generics;

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
                if let syn::Type::Path(p) = &field.ty {
                    for segment in &p.path.segments {
                        if segment.ident != "PhantomData" {
                            let ident = get_ultimate_identifier(&field.ty);                            
                            for generic_param in generics.params.iter_mut() {
                                if let syn::GenericParam::Type(generic_tp) = generic_param {                                    
                                    if &generic_tp.ident == ident {
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

    let (impl_generics, type_generics, where_clauses) = generics.split_for_impl();

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
        impl #impl_generics ::std::fmt::Debug for #structname #type_generics #where_clauses
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
