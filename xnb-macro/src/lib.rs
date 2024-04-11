use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, DataStruct, DeriveInput, Field,
    GenericParam, Generics, Ident, LitStr, PathArguments, Type, TypePath,
};

struct Error {
    msg: String,
    span: Span,
}

impl Error {
    fn new(msg: &str, span: Span) -> Self {
        Self {
            msg: msg.to_string(),
            span,
        }
    }
}

macro_rules! error {
    ($span:expr, $($args:expr),+) => {{
        Error::new(&format!($($args),+), $span)
    }}

}

type Result<T> = std::result::Result<T, Error>;
fn xnb_type_macro_impl(input: DeriveInput) -> Result<TokenStream> {
    // Used in the quasi-quotation below as `#name`.
    let name = &input.ident;

    let type_registration = get_type_registration(&input)?;
    // Add a bound `T: HeapSize` to every type parameter T.
    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let register_subtypes = match &input.data {
        syn::Data::Struct(ref data) => register_struct_field_types(data)?,
        syn::Data::Enum(_) => quote! {}, // Enums do not need to register sub types
        syn::Data::Union(_) => todo!(),
    };

    let fields = match &input.data {
        syn::Data::Struct(ref data) => struct_field_type_ids(data),
        syn::Data::Enum(_) => Vec::new(),
        syn::Data::Union(_) => Vec::new(),
    };

    let any_type = match &input.data {
        syn::Data::Struct(_) => quote! {xnb::AnyType::Struct},
        syn::Data::Enum(_) => quote! {xnb::AnyType::I32},
        syn::Data::Union(_) => todo!(),
    };

    let (xnb_name, nullable) = match type_registration {
        TypeRegistartion::NonNullable(name) => (name, false),
        TypeRegistartion::Nullable(name) => (name, true),
    };

    let tagged = !has_attr(&input.attrs, "xnb", &["untagged"]);

    // Build the output, possibly using quasi-quotation
    Ok(quote! {
        impl #impl_generics xnb::XnbType for #name #ty_generics #where_clause {
            fn register(registry: &mut xnb::TypeRegistry) -> xnb::Result<()> {
                #register_subtypes
                // TODO: register generic types and pass them to register_type
                registry.register_type(
                    xnb::TypeSpec{
                        name: #xnb_name.to_string(),
                        sub_types: vec![],
                        fields: vec![#(#fields),*],
                        nullable: #nullable,
                        tagged: #tagged,
                        any_type: #any_type,
                    },
                    std::any::TypeId::of::<Self>())?;
                Ok(())
            }
        }
    })
}

enum TypeRegistartion {
    NonNullable(String),
    Nullable(String),
}

fn get_type_registration(input: &DeriveInput) -> Result<TypeRegistartion> {
    match input.data {
        syn::Data::Struct(_) => struct_type_registration(input),
        syn::Data::Enum(_) => enum_type_registration(input),
        syn::Data::Union(_) => todo!(),
    }
}

fn enum_type_registration(input: &DeriveInput) -> Result<TypeRegistartion> {
    let repr_attr = input.attrs.iter().find(|attr| attr.path().is_ident("repr"));
    let Some(repr_attr) = repr_attr else {
        return Ok(TypeRegistartion::Nullable(
            "Microsoft.Xna.Framework.Content.StringReader".to_string(),
        ));
    };

    let repr_value = repr_attr
        .parse_args::<Ident>()
        .map_err(|e| error!(e.span(), "expected #[repr()] argument to be an ident: {e}"))?;

    match repr_value.to_string().as_str() {
        "i32" => Ok(TypeRegistartion::NonNullable(
            "Microsoft.Xna.Framework.Content.Int32Reader".to_string(),
        )),
        val => Err(error!(repr_value.span(), "Unknown type in #[repr({val})]")),
    }
}

fn struct_type_registration(input: &DeriveInput) -> Result<TypeRegistartion> {
    let xnb_name = input
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("xnb_name"))
        .ok_or_else(|| {
            error!(
                input.span(),
                "expected type to be annoated with `#[xnb_name(\"path\")]`"
            )
        })?;
    xnb_name
        .parse_args::<LitStr>()
        .map_err(|e| {
            error!(
                e.span(),
                "can't parse argument to `#[xnb_name(\"path\")]`: {e}"
            )
        })
        .map(|v| TypeRegistartion::Nullable(v.value()))
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(xnb::XnbType));
        }
    }
    generics
}

fn has_attr(attrs: &[Attribute], name: &str, possible_values: &[&str]) -> bool {
    attrs.iter().any(|attr| {
        if !attr.path().is_ident(name) {
            return false;
        }

        let Ok(arg) = attr.parse_args::<Ident>() else {
            return false;
        };

        let arg_value = arg.to_string();

        possible_values
            .iter()
            .any(|possible| *possible == arg_value)
    })
}

fn has_skip_attr(field: &Field) -> bool {
    has_attr(&field.attrs, "serde", &["skip", "skip_deserializing"])
}

fn register_struct_field_types(data: &DataStruct) -> Result<TokenStream> {
    match data.fields {
        syn::Fields::Named(ref fields) => {
            let field_fragments = fields.named.iter().filter_map(|f| {
                if has_skip_attr(f) {
                    return None;
                }
                let Type::Path(path) = &f.ty else {
                    return None;
                };
                let segments = to_turbofish(path);
                Some(quote_spanned! {f.span() => #(#segments)*::register(registry)?;})
            });

            Ok(quote! {
                #(#field_fragments)*
            })
        }
        syn::Fields::Unnamed(_) => todo!(),
        syn::Fields::Unit => todo!(),
    }
}

fn struct_field_type_ids(data: &DataStruct) -> Vec<TokenStream> {
    match data.fields {
        syn::Fields::Named(ref fields) => fields
            .named
            .iter()
            .filter_map(|f| {
                if has_skip_attr(f) {
                    return None;
                }
                let ty = &f.ty;
                let name = f
                    .ident
                    .as_ref()
                    .map(|ident| ident.to_string())
                    .unwrap_or("".to_string());
                let inline = has_attr(&f.attrs, "serde", &["flatten"]);
                Some(quote_spanned! { f.span() =>
                    xnb::FieldSpec{
                        name: #name.to_string(),
                        type_id: std::any::TypeId::of::<#ty>(),
                        inline: #inline,
                    }
                })
            })
            .collect(),
        syn::Fields::Unnamed(_) => todo!(),
        syn::Fields::Unit => todo!(),
    }
}

fn to_turbofish(path: &TypePath) -> Vec<TokenStream> {
    assert!(path.qself.is_none());
    assert!(path.path.leading_colon.is_none());
    path.path
        .segments
        .iter()
        .fold(Vec::new(), |mut acc, segment| {
            let ident = &segment.ident;
            if !acc.is_empty() {
                acc.push(quote! {::});
            }
            acc.push(quote! {#ident});
            if let PathArguments::AngleBracketed(args) = &segment.arguments {
                acc.push(quote! {::#args});
            }
            acc
        })
}

#[proc_macro_derive(XnbType)]
pub fn xnb_type_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    let output = xnb_type_macro_impl(input)
        .unwrap_or_else(|e| syn::parse::Error::new(e.span, e.msg).into_compile_error());

    // Hand the output tokens back to the compiler
    proc_macro::TokenStream::from(output)
}

#[proc_macro_attribute]
pub fn xnb_name(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
pub fn xnb_untagged(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[cfg(test)]
mod tests {}
