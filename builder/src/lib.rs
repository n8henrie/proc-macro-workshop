use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput,
    GenericArgument, Ident, Lit, Meta, MetaNameValue, Path, PathArguments, PathSegment, Type,
    TypePath,
};

enum FieldType<'a> {
    Optional(&'a Type),
    Required(&'a Type),
}

struct BuilderField<'a> {
    name: &'a Option<Ident>,
    fieldtype: FieldType<'a>,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let structname = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", structname), Span::call_site());

    let fields = match input.data {
        Data::Struct(DataStruct {
            fields: fieldsnamed,
            ..
        }) => fieldsnamed,
        _ => panic!("No data struct"),
    };

    let struct_fields = fields
        .iter()
        .map(|field| {
            let attrs = &field.attrs;

            let _attr_val = attrs.first().map(|attr| {
                attr.path.segments.first().map(|PathSegment { ident, .. }| {
                    if ident == "builder" {
                        if let Ok(Meta::NameValue(MetaNameValue {
                            lit: Lit::Str(litstr),
                            ..
                        })) = attr.parse_args()
                        {
                            Some(litstr.value())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            });

            let name = &field.ident;
            let fieldtype = match &field.ty {
                Type::Path(TypePath {
                    path: Path { segments, .. },
                    ..
                }) => match segments.first() {
                    Some(PathSegment {
                        ident,
                        arguments:
                            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                args, ..
                            }),
                    }) if ident == "Option" => match args.first() {
                        Some(GenericArgument::Type(t)) => Some(FieldType::Optional(t)),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            }
            .unwrap_or(FieldType::Required(&field.ty));
            BuilderField { name, fieldtype }
        })
        .collect::<Vec<_>>();

    let builder_fields = struct_fields
        .iter()
        .map(|BuilderField { name, fieldtype }| {
            let ty = match *fieldtype {
                FieldType::Optional(ty) => ty,
                FieldType::Required(ty) => ty,
            };
            quote! { #name: ::std::option::Option::<#ty> }
        });

    let methods = struct_fields
        .iter()
        .map(|BuilderField { name, fieldtype }| {
            let ty = match *fieldtype {
                FieldType::Optional(ty) => ty,
                FieldType::Required(ty) => ty,
            };
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        });

    let output_fields =
        struct_fields
            .iter()
            .map(|BuilderField { name, fieldtype }| match *fieldtype {
                FieldType::Optional(_) => quote! { #name: self.#name.take() },
                FieldType::Required(_) => quote! { #name: self.#name.take().ok_or_else(|| format!("required value is unset: {}", stringify!(#name)))? },
            });

    (quote! {
        impl #structname {
            pub fn builder() -> #builder_name {
                #builder_name {
                    ..Default::default()
                }
            }
        }

    #[derive(Default)]
    pub struct #builder_name {
         #(#builder_fields),*
     }

     impl #builder_name {
         #(#methods)*
        pub fn build(&mut self) -> Result<#structname, Box<dyn std::error::Error>> {
            Ok(#structname{
                #(#output_fields),*
            })
        }
     }
    })
    .into()
}
