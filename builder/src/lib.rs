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
    Repeater((Ident, &'a Type)),
}

struct BuilderField<'a> {
    dest: &'a Option<Ident>,
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

            let attr_val: Option<String> = attrs.first().and_then(|attr| {
                attr.path
                    .segments
                    .first()
                    .and_then(|PathSegment { ident, .. }| {
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
            let fieldtype = if let Type::Path(TypePath {
                path: Path { segments, .. },
                ..
            }) = &field.ty
            {
                if let Some(PathSegment {
                    ident,
                    arguments:
                        PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                }) = segments.first()
                {
                    match ident.to_string().as_ref() {
                        "Option" => {
                            if let Some(GenericArgument::Type(t)) = args.first() {
                                Some(FieldType::Optional(t))
                            } else {
                                None
                            }
                        }
                        "Vec" => {
                            if let Some(GenericArgument::Type(t)) = args.first() {
                                if let Some(attr_val) = attr_val {
                                    let ident = Ident::new(&attr_val, Span::call_site());
                                    Some(FieldType::Repeater((ident, t)))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            } else {
                None
            }
            .unwrap_or(FieldType::Required(&field.ty));
            BuilderField {
                dest: name,
                fieldtype,
            }
        })
        .collect::<Vec<_>>();

    let builder_fields = struct_fields.iter().map(
        |BuilderField {
             dest, fieldtype, ..
         }| {
            match *fieldtype {
                FieldType::Optional(ty) | FieldType::Required(ty) => {
                    quote! { #dest: ::std::option::Option::<#ty> }
                }
                FieldType::Repeater((_, ty)) => {
                    quote! { #dest: ::std::vec::Vec::<#ty> }
                }
            }
        },
    );

    let methods = struct_fields
        .iter()
        .map(|BuilderField { dest, fieldtype }| match fieldtype {
            FieldType::Optional(ty) | FieldType::Required(ty) => {
                quote! {
                    fn #dest(&mut self, #dest: #ty) -> &mut Self {
                        self.#dest = Some(#dest);
                        self
                    }
                }
            }
            FieldType::Repeater((ident, ty)) => {
                quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#dest.push(#ident);
                        self
                    }
                }
            }
        });

    let output_fields =
        struct_fields
            .iter()
            .map(|BuilderField { dest: name, fieldtype, ..}| match *fieldtype {
                FieldType::Optional(_) => quote! { #name: self.#name.take() },
                FieldType::Repeater(_) => quote! { #name: self.#name.drain(..).collect() },
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
