use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as Tokens};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{
    parse_macro_input,
    parse_quote,
    DeriveInput,
    Error,
    Expr,
    LitStr,
    Path,
    Result as SynResult,
};

#[derive(Default)]
struct Options {
    runtime: bool,
    name: Option<LitStr>,
    crate_path: Option<Path>,
    display: bool,
    json: bool,
    serializable: bool,
    hash: bool,
    eq: bool,
    hash_cost: Option<Expr>,
    eq_cost: Option<Expr>,
    skip_json: bool,
    size: Option<Expr>,
    children: Option<Expr>,
    children_mut: Option<Expr>,
    deep_clone: Option<Expr>,
    deep_clone_in: Option<Expr>,
}

fn options(input: &DeriveInput, type_only: bool) -> SynResult<Options> {
    let mut options = Options::default();
    let mut seen = HashSet::new();
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
                "runtime" if !type_only => options.runtime = true,
                "display" if !type_only => options.display = true,
                "json" if !type_only => options.json = true,
                "serializable" if !type_only => options.serializable = true,
                "hash" if !type_only => options.hash = true,
                "eq" if !type_only => options.eq = true,
                "hash_cost" if !type_only => {
                    options.hash = true;
                    options.hash_cost = Some(meta.value()?.parse()?);
                }
                "eq_cost" if !type_only => {
                    options.eq = true;
                    options.eq_cost = Some(meta.value()?.parse()?);
                }
                "skip_json" if !type_only => options.skip_json = true,
                "size" if !type_only => options.size = Some(meta.value()?.parse()?),
                "children" if !type_only => options.children = Some(meta.value()?.parse()?),
                "children_mut" if !type_only => options.children_mut = Some(meta.value()?.parse()?),
                "deep_clone" if !type_only => options.deep_clone = Some(meta.value()?.parse()?),
                "deep_clone_in" if !type_only => options.deep_clone_in = Some(meta.value()?.parse()?),
                _ => return Err(meta.error("unsupported opaque option")),
            }
            Ok(())
        })?;
    }
    if options.json && options.skip_json {
        return Err(Error::new_spanned(
            input,
            "json and skip_json cannot be combined",
        ));
    }
    Ok(options)
}

fn types_path(options: &Options) -> SynResult<Tokens> {
    if let Some(path) = &options.crate_path {
        return Ok(quote!(#path));
    }
    match crate_name("silex-types") {
        Ok(FoundCrate::Itself) => Ok(quote!(::silex_types)),
        Ok(FoundCrate::Name(name)) => {
            let ident = format_ident!("{}", name);
            Ok(quote!(::#ident))
        }
        Err(error) => Err(Error::new(Span::call_site(), error)),
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
        const _: () = {
            use #path::traits::DynType as __SilexDynType;
            use ::std::any::TypeId as __SilexTypeId;

            impl #impl_generics __SilexDynType for #ident #type_generics #where_clause {
                fn get_type_name(&self) -> &'static str {
                    #name
                }

                fn get_type(&self) -> __SilexTypeId {
                    __SilexTypeId::of::<Self>()
                }
            }
        };
    }
}

fn expand(input: &DeriveInput, type_only: bool) -> SynResult<Tokens> {
    let options = options(input, type_only)?;
    let path = types_path(&options)?;
    let type_impl = dyn_type(input, &options, &path);
    if type_only {
        return Ok(type_impl);
    }

    let ident = &input.ident;
    let mut generics = input.generics.clone();
    generics.make_where_clause().predicates.push(parse_quote!(
        Self: 'static + __SilexClone + __SilexDebug
    ));
    if !options.runtime {
        generics.make_where_clause().predicates.push(parse_quote!(
            Self: __SilexSend + __SilexSync
        ));
    }
    if options.hash {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: __SilexDynHash));
    }
    if options.eq {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: __SilexDynEq));
    }
    if options.display {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: __SilexDisplay));
    }
    if options.serializable {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: __SilexDynSerializable));
    }
    if options.json {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: __SilexSerialize));
    }
    if options.skip_json {
        generics
            .make_where_clause()
            .predicates
            .push(parse_quote!(Self: __SilexJSONHelper));
    }
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let json_impl = if options.skip_json {
        quote!()
    } else if options.json {
        quote! {
            impl #impl_generics __SilexJSONHelper for #ident #type_generics #where_clause {
                fn serialize_json(&self) -> __SilexResult<__SilexJsonValue, __SilexError> {
                    use #path::__private::matriochka::ResultExt as _;

                    #path::__private::serde_json::to_value(self)
                        .with_context(|| ::std::format!(
                            "serializing opaque type {} to JSON",
                            __SilexDynType::get_type_name(self),
                        ))
                }

                fn is_json_supported(&self) -> bool { true }
            }
        }
    } else {
        quote! {
            impl #impl_generics __SilexJSONHelper for #ident #type_generics #where_clause {}
        }
    };
    let display = if options.display {
        quote! {
            fn display<'fmt>(&self, f: &mut __SilexFormatter<'fmt>) -> __SilexFmtResult {
                ::std::write!(f, "{}", self)
            }
        }
    } else {
        quote! {
            fn display<'fmt>(&self, f: &mut __SilexFormatter<'fmt>) -> __SilexFmtResult {
                ::std::write!(f, "Opaque({})", __SilexDynType::get_type_name(self))
            }
        }
    };
    let serializable = options.serializable.then(|| {
        quote! {
            fn as_serializable(&self) -> Option<&dyn __SilexDynSerializable> { Some(self) }
        }
    });
    let hash = options.hash.then(|| {
        quote! {
            fn as_hashable(&self) -> Option<&dyn __SilexDynHash> { Some(self) }
        }
    });
    let eq = options.eq.then(|| {
        quote! {
            fn as_eq(&self) -> Option<&dyn __SilexDynEq> { Some(self) }
        }
    });
    let hash_cost = options.hash_cost.map(|cost| quote! {
        fn pre_hash_cost(&self, gas: &mut __SilexGasMeter) -> __SilexResult<(), __SilexValueError> {
            Ok(gas.charge(#cost)?)
        }
    });
    let eq_cost = options.eq_cost.map(|cost| quote! {
        fn pre_eq_cost(&self, gas: &mut __SilexGasMeter) -> __SilexResult<(), __SilexValueError> {
            Ok(gas.charge(#cost)?)
        }
    });
    let size = options.size.map(|size| {
        quote! {
            fn get_size(&self) -> Result<usize, __SilexError> { Ok(#size) }
        }
    });
    let deep_clone = options
        .deep_clone
        .unwrap_or_else(|| parse_quote!(__SilexClone::clone(self)));
    let deep_clone_in = options.deep_clone_in.map(|expression| quote! {
        fn deep_clone_in(&self, heap: &mut __SilexHeap) -> __SilexResult<__SilexBox<dyn __SilexRuntimeOpaque>, __SilexHeapError> {
            Ok(__SilexBox::new(#expression))
        }
    });
    let children = options.children.map(|children| {
        quote! {
            fn visit_children<'a>(&'a self, visitor: &mut dyn FnMut(__SilexOpaqueChild<'a>)) {
                for child in #children { visitor(__SilexInto::into(child)); }
            }
        }
    });
    let children_mut = options.children_mut.map(|expression| quote! {
        fn visit_children_mut(&mut self, visitor: &mut dyn for<'child> FnMut(__SilexOpaqueChildMut<'child>) -> __SilexResult<(), __SilexHeapError>) -> __SilexResult<bool, __SilexHeapError> {
            #expression?;
            Ok(true)
        }
    });
    let cloning = if options.runtime {
        quote! {
            impl #impl_generics __SilexRuntimeOpaque for #ident #type_generics #where_clause {
                fn clone_runtime(&self) -> __SilexBox<dyn __SilexRuntimeOpaque> {
                    __SilexBox::new(__SilexClone::clone(self))
                }
                #deep_clone_in
                #children_mut
            }
        }
    } else {
        if deep_clone_in.is_some() || children_mut.is_some() {
            return Err(Error::new_spanned(input, "heap-aware cloning requires #[opaque(runtime)]"));
        }
        quote! {
            impl #impl_generics __SilexOpaque for #ident #type_generics #where_clause {
                fn clone_box(&self) -> __SilexBox<dyn __SilexOpaque> {
                    __SilexBox::new(__SilexClone::clone(self))
                }

                fn deep_clone(&self) -> __SilexBox<dyn __SilexOpaque> {
                    __SilexBox::new(#deep_clone)
                }
            }
        }
    };
    Ok(quote! {
        // Scoped imports avoid leaking generated names into the caller's module.
        const _: () = {
            #[allow(unused_imports)]
            use ::std::{
                boxed::Box as __SilexBox,
                clone::Clone as __SilexClone,
                convert::Into as __SilexInto,
                fmt::{
                    Debug as __SilexDebug,
                    Display as __SilexDisplay,
                    Formatter as __SilexFormatter,
                    Result as __SilexFmtResult,
                },
                marker::{Send as __SilexSend, Sync as __SilexSync},
                result::Result as __SilexResult,
            };
            #[allow(unused_imports)]
            use #path::{
                GasMeter as __SilexGasMeter,
                Heap as __SilexHeap,
                HeapError as __SilexHeapError,
                Opaque as __SilexOpaque,
                OpaqueChild as __SilexOpaqueChild,
                OpaqueChildMut as __SilexOpaqueChildMut,
                OpaqueValue as __SilexOpaqueValue,
                RuntimeOpaque as __SilexRuntimeOpaque,
                ValueError as __SilexValueError,
                traits::{
                    DynEq as __SilexDynEq,
                    DynHash as __SilexDynHash,
                    DynSerializable as __SilexDynSerializable,
                    DynType as __SilexDynType,
                    JSONHelper as __SilexJSONHelper,
                },
                __private::{
                    matriochka::Error as __SilexError,
                    serde::Serialize as __SilexSerialize,
                    serde_json::Value as __SilexJsonValue,
                },
            };

            #type_impl
            #json_impl
            #cloning
            impl #impl_generics __SilexOpaqueValue for #ident #type_generics #where_clause {
                #display
                #serializable
                #hash
                #eq
                #hash_cost
                #eq_cost
                #size
                #children
            }
        };
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
/// - `hash_cost = expression`: enable hashing and charge the given `u64` gas
///   before each metered hash.
/// - `eq_cost = expression`: enable equality and charge the given `u64` gas
///   before each metered comparison, including map collisions.
///   Both expressions can refer to `self`. The macro implements the
///   `OpaqueValue::pre_hash_cost` and `OpaqueValue::pre_eq_cost` hooks for
///   the options provided.
/// - `size = expression`: override the size used for value accounting.
/// - `deep_clone = expression`: produce an independent `Self` with retained VM
///   owned values deeply cloned. Defaults to `Clone`.
/// - `runtime`: implement `RuntimeOpaque` for an execution-local type instead
///   of portable `Opaque`. The type may contain runtime handles and cannot be
///   stored in `OwnedValue`.
/// - `deep_clone_in = expression`: with `runtime`, clone payloads using the mutable
///   heap available as `heap`. Containers retaining heap handles must
///   override this to detach their payloads. The expression produces `Self`
///   and can use `?` to propagate `HeapError`.
/// - `children_mut = expression`: expose mutable runtime children for iterative
///   heap detachment; the expression receives `visitor` and returns a Result.
/// - `children = expression`: an iterable of direct children convertible to
///   `OpaqueChild` for bounded validation and depth checking. Use `&RuntimeValue`
///   (for example, `&self.values`), `&ValuePointer`, `&OwnedValue`, `&StackValue`,
///   `&dyn Opaque`, or `OpaqueChild` to mix them.
/// - `skip_json`: keep an existing custom `JSONHelper` implementation.
/// - `crate = "path"`: override the automatically resolved `silex-types` path.
///
/// `Clone` and `Debug` are required. Portable types also require `Send` and
/// `Sync`. Generic parameters and where clauses are preserved.
///
/// ```ignore
/// use serde::Serialize;
/// use silex_types::Opaque;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Opaque)]
/// #[opaque(name = "Account", display, json, serializable)]
/// struct Account { /* ... */ }
/// // Implement Display and octet::Serializable separately.
/// ```
#[proc_macro_derive(Opaque, attributes(opaque))]
pub fn derive_opaque(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(&input, false)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

/// Derive type identity only, for types with a custom `Opaque` implementation.
#[proc_macro_derive(DynType, attributes(opaque))]
pub fn derive_dyn_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(&input, true)
        .unwrap_or_else(Error::into_compile_error)
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
