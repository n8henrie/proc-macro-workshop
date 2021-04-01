use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DeriveInput};

struct DebugField<T: ToTokens> {
    named: T,
    format: Option<String>,
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let parsed_input = parse_macro_input!(input as DeriveInput);

    let structname = parsed_input.ident;
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

    (quote! {
        impl ::std::fmt::Debug for #structname {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result  {
                fmt.debug_struct(stringify!(#structname))
                    #(#output_fields)*
                    .finish()
            }
        }
    })
    .into()
}
