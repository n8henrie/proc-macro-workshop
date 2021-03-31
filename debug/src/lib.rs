use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let parsed_input = parse_macro_input!(input as DeriveInput);

    let structname = parsed_input.ident;
    let fields = match parsed_input.data {
        syn::Data::Struct(
            syn::DataStruct {
                fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
                ..
            },
            ..,
        ) => named,
        _ => panic!("no fields!"),
    };

    let output_fields = fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            quote! { .field(stringify!(#ident), &self.#ident) }
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
