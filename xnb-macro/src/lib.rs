use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, DataStruct, DeriveInput, GenericParam,
    Generics, Ident, LitStr, PathArguments, Type, TypePath,
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

    let xnb_name = get_xnb_name(&input)?;
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
                    },
                    std::any::TypeId::of::<Self>())?;
                Ok(())
            }
        }
    })
}

fn get_xnb_name(input: &DeriveInput) -> Result<String> {
    match input.data {
        syn::Data::Struct(_) => xnb_struct_name(input),
        syn::Data::Enum(_) => xnb_enum_name(input),
        syn::Data::Union(_) => todo!(),
    }
}

fn xnb_enum_name(input: &DeriveInput) -> Result<String> {
    let repr_attr = input.attrs.iter().find(|attr| attr.path().is_ident("repr"));
    let Some(repr_attr) = repr_attr else {
        return Ok("Microsoft.Xna.Framework.Content.StringReader".to_string());
    };

    let repr_value = repr_attr
        .parse_args::<Ident>()
        .map_err(|e| error!(e.span(), "expected #[repr()] argument to be an ident: {e}"))?;

    match repr_value.to_string().as_str() {
        "i32" => Ok("Microsoft.Xna.Framework.Content.Int32Reader".to_string()),
        val => Err(error!(repr_value.span(), "Unknown type in #[repr({val})]")),
    }
}

fn xnb_struct_name(input: &DeriveInput) -> Result<String> {
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
        .map(|v| v.value())
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(xnb::XnbType));
        }
    }
    generics
}

fn register_struct_field_types(data: &DataStruct) -> Result<TokenStream> {
    match data.fields {
        syn::Fields::Named(ref fields) => {
            let field_fragments = fields.named.iter().map(|f| {
                if let Type::Path(path) = &f.ty {
                    let segments = to_turbofish(path);
                    quote_spanned! {f.span() => #(#segments)*::register(registry)?;}
                } else {
                    quote! {}
                }
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
            .map(|f| {
                let ty = &f.ty;
                let name = f
                    .ident
                    .as_ref()
                    .map(|ident| ident.to_string())
                    .unwrap_or("".to_string());
                quote_spanned! {f.span() => (#name.to_string(), std::any::TypeId::of::<#ty>())}
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

#[cfg(test)]
mod tests {}
