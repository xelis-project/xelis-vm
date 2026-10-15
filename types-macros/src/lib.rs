use proc_macro::TokenStream;
use proc_macro2::TokenStream as Tokens;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{parse_macro_input, parse_quote, DeriveInput, Expr, LitStr, Path};

#[derive(Default)]
struct Options {
    name: Option<LitStr>,
    crate_path: Option<Path>,
    display: bool,
    json: bool,
    serializable: bool,
    hash: bool,
    eq: bool,
    skip_json: bool,
    size: Option<Expr>,
    children: Option<Expr>,
    deep_clone: Option<Expr>,
}

fn options(input: &DeriveInput, type_only: bool) -> syn::Result<Options> {
    let mut options = Options::default();
    let mut seen = std::collections::HashSet::new();
    for attr in input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("opaque"))
    {
        attr.parse_nested_meta(|meta| {
            let Some(key) = meta.path.get_ident().map(ToString::to_string) else {
                return Err(meta.error("expected an opaque option"));
            };
            if !seen.insert(key.clone()) {
                return Err(meta.error("duplicate opaque option"));
            }
            match key.as_str() {
                "name" => options.name = Some(meta.value()?.parse()?),
                "crate" => {
                    let path: LitStr = meta.value()?.parse()?;
                    options.crate_path = Some(path.parse()?);
                }
                "display" if !type_only => options.display = true,
                "json" if !type_only => options.json = true,
                "serializable" if !type_only => options.serializable = true,
                "hash" if !type_only => options.hash = true,
                "eq" if !type_only => options.eq = true,
                "skip_json" if !type_only => options.skip_json = true,
                "size" if !type_only => options.size = Some(meta.value()?.parse()?),
                "children" if !type_only => options.children = Some(meta.value()?.parse()?),
                "deep_clone" if !type_only => options.deep_clone = Some(meta.value()?.parse()?),
                _ => return Err(meta.error("unsupported opaque option")),
            }
            Ok(())
        })?;
    }
    if options.json && options.skip_json {
        return Err(syn::Error::new_spanned(
            input,
            "json and skip_json cannot be combined",
        ));
    }
    Ok(options)
}

fn types_path(options: &Options) -> syn::Result<Tokens> {
    if let Some(path) = &options.crate_path {
        return Ok(quote!(#path));
    }
    match crate_name("silex-types") {
        Ok(FoundCrate::Itself) => Ok(quote!(::silex_types)),
        Ok(FoundCrate::Name(name)) => {
            let ident = format_ident!("{}", name);
            Ok(quote!(::#ident))
        }
        Err(error) => Err(syn::Error::new(proc_macro2::Span::call_site(), error)),
    }
}

fn dyn_type(input: &DeriveInput, options: &Options, path: &Tokens) -> Tokens {
    let ident = &input.ident;
    let name = options
        .name
        .clone()
        .unwrap_or_else(|| LitStr::new(&ident.to_string(), ident.span()));
    let mut generics = input.generics.clone();
    generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(Self: 'static));
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    quote! {
        impl #impl_generics #path::traits::DynType for #ident #type_generics #where_clause {
            fn get_type_name(&self) -> &'static str { #name }
            fn get_type(&self) -> ::std::any::TypeId { ::std::any::TypeId::of::<Self>() }
        }
    }
}

fn expand(input: &DeriveInput, type_only: bool) -> syn::Result<Tokens> {
    let options = options(input, type_only)?;
    let path = types_path(&options)?;
    let type_impl = dyn_type(input, &options, &path);
    if type_only {
        return Ok(type_impl);
    }

    let ident = &input.ident;
    let mut generics = input.generics.clone();
    generics.make_where_clause().predicates.push(parse_quote!(
        Self: 'static + ::std::clone::Clone + ::std::fmt::Debug + ::std::marker::Send
            + ::std::marker::Sync
    ));
    if options.hash {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: #path::traits::DynHash));
    }
    if options.eq {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: #path::traits::DynEq));
    }
    if options.display {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: ::std::fmt::Display));
    }
    if options.serializable {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: #path::traits::DynSerializable));
    }
    if options.json {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: #path::__private::serde::Serialize));
    }
    if options.skip_json {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: #path::traits::JSONHelper));
    }
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let json_impl = if options.skip_json {
        quote!()
    } else if options.json {
        quote! {
            impl #impl_generics #path::traits::JSONHelper for #ident #type_generics #where_clause {
                fn serialize_json(&self) -> #path::__private::anyhow::Result<#path::__private::serde_json::Value> {
                    Ok(#path::__private::serde_json::to_value(self)?)
                }
                fn is_json_supported(&self) -> bool { true }
            }
        }
    } else {
        quote! {
            impl #impl_generics #path::traits::JSONHelper for #ident #type_generics #where_clause {}
        }
    };
    let display = if options.display {
        quote! {
            fn display(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                ::std::write!(f, "{}", self)
            }
        }
    } else {
        quote! {
            fn display(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                ::std::write!(f, "Opaque({})", #path::traits::DynType::get_type_name(self))
            }
        }
    };
    let serializable = options.serializable.then(|| {
        quote! {
            fn as_serializable(&self) -> Option<&dyn #path::traits::DynSerializable> { Some(self) }
        }
    });
    let hash = options.hash.then(|| {
        quote! {
            fn as_hashable(&self) -> Option<&dyn #path::traits::DynHash> { Some(self) }
        }
    });
    let eq = options.eq.then(|| {
        quote! {
            fn as_eq(&self) -> Option<&dyn #path::traits::DynEq> { Some(self) }
        }
    });
    let size = options.size.map(|size| {
        quote! {
            fn get_size(&self) -> usize { #size }
        }
    });
    let deep_clone = options
        .deep_clone
        .unwrap_or_else(|| parse_quote!(::std::clone::Clone::clone(self)));
    let children = options.children.map(|children| {
        quote! {
            fn visit_children<'a>(&'a self, visitor: &mut dyn FnMut(#path::OpaqueChild<'a>)) {
                for child in #children { visitor(::core::convert::Into::into(child)); }
            }
        }
    });
    Ok(quote! {
        #type_impl
        #json_impl
        impl #impl_generics #path::Opaque for #ident #type_generics #where_clause {
            fn clone_box(&self) -> ::std::boxed::Box<dyn #path::Opaque> {
                ::std::boxed::Box::new(::std::clone::Clone::clone(self))
            }
            fn deep_clone(&self) -> ::std::boxed::Box<dyn #path::Opaque> {
                ::std::boxed::Box::new(#deep_clone)
            }
            #display
            #serializable
            #hash
            #eq
            #size
            #children
        }
    })
}

/// Derive `Opaque`, `DynType`, and a default unsupported `JSONHelper`.
///
/// Options in `#[opaque(...)]`:
/// - `name = "Name"`: runtime type name (defaults to the Rust type name).
/// - `display`: forward opaque formatting to the type's `Display` implementation.
/// - `json`: serialize through Serde and enable JSON support.
/// - `serializable`: expose the binary serialization capability. Types implementing
///   Octet's `Serializable` automatically implement the object-safe adapter.
/// - `hash`: expose hashing through `Hash` or a custom `DynHash` implementation.
/// - `eq`: expose equality through `PartialEq` or a custom `DynEq` implementation.
///   Map keys require both `hash` and `eq`.
/// - `size = expression`: override the size used for value accounting.
/// - `deep_clone = expression`: produce an independent `Self` with retained VM
///   values deeply cloned. Defaults to `Clone`; containers retaining VM pointers
///   must override this to detach their payloads.
/// - `children = expression`: an iterable of direct children convertible to
///   `OpaqueChild` for bounded validation and depth checking. Use `&ValueCell`
///   (for example, `&self.values`), `&dyn Opaque`, or `OpaqueChild` to mix both.
/// - `skip_json`: keep an existing custom `JSONHelper` implementation.
/// - `crate = "path"`: override the automatically resolved `silex-types` path.
///
/// `Clone`, `Debug`, `Send`, and `Sync` must be provided by the
/// type. Generic parameters and where clauses are preserved.
///
/// ```ignore
/// #[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, silex_types::Opaque)]
/// #[opaque(name = "Account", display, json, serializable)]
/// struct Account { /* ... */ }
/// // Implement Display and octet::Serializable separately.
/// ```
#[proc_macro_derive(Opaque, attributes(opaque))]
pub fn derive_opaque(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(&input, false)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

/// Derive type identity only, for types with a custom `Opaque` implementation.
#[proc_macro_derive(DynType, attributes(opaque))]
pub fn derive_dyn_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(&input, true)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_invalid_options() {
        for input in [
            quote!(
                #[opaque(unknown)]
                struct Example;
            ),
            quote!(
                #[opaque(json, skip_json)]
                struct Example;
            ),
            quote!(
                #[opaque(display, display)]
                struct Example;
            ),
            quote!(
                #[opaque(name = 42)]
                struct Example;
            ),
            quote!(
                #[opaque(serializable = false)]
                struct Example;
            ),
        ] {
            let input: DeriveInput = syn::parse2(input).unwrap();
            assert!(options(&input, false).is_err());
        }
    }

    #[test]
    fn type_identity_only_accepts_identity_options() {
        let input: DeriveInput = parse_quote!(
            #[opaque(name = "Example")]
            struct Example;
        );
        assert!(options(&input, true).is_ok());
        let input: DeriveInput = parse_quote!(
            #[opaque(json)]
            struct Example;
        );
        assert!(options(&input, true).is_err());
    }
}
