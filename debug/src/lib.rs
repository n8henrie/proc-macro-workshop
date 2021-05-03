use proc_macro::TokenStream;
use quote::{quote, ToTokens};

struct DebugField<T: ToTokens> {
    named: T,
    format: Option<String>,
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);

    let structname = parsed_input.ident;
    let generics = add_trait_bounds(parsed_input.generics);

    let (impl_generics, type_generics, where_clauses) = generics.split_for_impl();

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

// Add a bound `T: HeapSize` to every type parameter T.
fn add_trait_bounds(mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            type_param
                .bounds
                .push(syn::parse_quote!(::std::fmt::Debug));
        }
    }
    generics
}