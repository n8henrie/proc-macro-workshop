use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Error,
    GenericArgument, Ident, Lit, Meta, MetaList, MetaNameValue, NestedMeta, Path, PathArguments,
    PathSegment, Type, TypePath,
};

enum FieldType<'a, T: ToTokens> {
    Optional(&'a Type),
    Required(&'a Type),
    Repeater((Ident, &'a Type)),
    MalFormed(T),
}

struct BuilderField<'a, T: ToTokens> {
    dest: &'a Option<Ident>,
    fieldtype: FieldType<'a, T>,
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

            let attr_val = (|| {
                if let Some(attr) = attrs.first() {
                    if let Ok(Meta::List(metalist)) = attr.parse_meta() {
                        let MetaList {
                            ref path,
                            ref nested,
                            ..
                        } = metalist;
                        if path.is_ident("builder") {
                            if let Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                                path,
                                lit,
                                ..
                            }))) = nested.first()
                            {
                                if path.is_ident("each") {
                                    if let Lit::Str(s) = lit {
                                        return Ok(Some(s.value()));
                                    };
                                };
                                return Err(metalist);
                            };
                        };
                    };
                };
                Ok(None)
            })();

            let name = &field.ident;
            let fieldtype = (|| {
                if let Type::Path(TypePath {
                    path: Path { segments, .. },
                    ..
                }) = &field.ty
                {
                    if let Some(PathSegment {
                        ident,
                        arguments:
                            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                args, ..
                            }),
                    }) = segments.first()
                    {
                        match ident {
                            _ if ident == "Option" => {
                                if let Some(GenericArgument::Type(t)) = args.first() {
                                    return Some(FieldType::Optional(t));
                                }
                            }
                            _ if ident == "Vec" => {
                                if let Some(GenericArgument::Type(t)) = args.first() {
                                    match attr_val {
                                        Ok(Some(attr_val)) => {
                                            let ident = Ident::new(&attr_val, Span::call_site());
                                            return Some(FieldType::Repeater((ident, t)));
                                        }
                                        Err(t) => {
                                            return Some(FieldType::MalFormed(t));
                                        }
                                        _ => (),
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                }
                None
            })()
            .unwrap_or(FieldType::Required(&field.ty));
            BuilderField {
                dest: name,
                fieldtype,
            }
        })
        .collect::<Vec<_>>();

    let builder_fields = match struct_fields
        .iter()
        .map(
            |BuilderField {
                 dest, fieldtype, ..
             }| {
                match fieldtype {
                    FieldType::Optional(ty) | FieldType::Required(ty) => {
                        Ok(quote! { #dest: ::std::option::Option::<#ty> })
                    }
                    FieldType::Repeater((_, ty)) => Ok(quote! { #dest: ::std::vec::Vec::<#ty> }),
                    FieldType::MalFormed(t) => {
                        Err(Error::new_spanned(t, r#"expected `builder(each = "...")`"#)
                            .to_compile_error())
                    }
                }
            },
        )
        .collect::<Result<Vec<_>, _>>()
    {
        Ok(fields) => fields,
        Err(err) => return err.into(),
    };

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
            _ => quote!(),
        });

    let output_fields =
        struct_fields
            .iter()
            .map(|BuilderField { dest: name, fieldtype, ..}| match *fieldtype {
                FieldType::Optional(_) => quote! { #name: self.#name.take() },
                FieldType::Repeater(_) => quote! { #name: self.#name.drain(..).collect() },
                FieldType::Required(_) => quote! { #name: self.#name.take().ok_or_else(|| format!("required value is unset: {}", stringify!(#name)))? },
                _ => unreachable!(),
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
        pub fn build(&mut self) -> ::std::result::Result<#structname, ::std::boxed::Box<dyn ::std::error::Error>> {
            Ok(#structname{
                #(#output_fields),*
            })
        }
     }
    })
    .into()
}
