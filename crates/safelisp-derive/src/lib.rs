//! Derive macros for converting Rust structs and enums to and from Safelisp
//! values.
//!
//! See [`safelisp::SafelispValue`] and [`safelisp::SafelispType`] for the
//! traits this macro implements.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
  parse_macro_input, Attribute, Data, DeriveInput, Expr, ExprLit, Fields, Ident, Lit, Meta, Type,
};

/// Derive `SafelispValue` and `SafelispType` for a Rust struct or enum.
///
/// The type must be non-generic and carry a `#[safelisp(module = "name")]`
/// attribute naming the Safelisp module the type belongs to. Every field type
/// must itself implement `SafelispValue`; the `safelisp` crate provides impls
/// for the integer and float primitives, `bool`, `String`, `()`, `Box<T>`, and
/// `Vec<T>`.
///
/// ```rust,ignore
/// use safelisp::SafelispValue;
///
/// #[derive(SafelispValue)]
/// #[safelisp(module = "arp")]
/// enum Dice {
///   Expr { num: u8, size: u8 },
///   Plus(
///     #[safelisp(field = "left")] Box<Dice>,
///     #[safelisp(field = "right")] Box<Dice>,
///   ),
///   Flat { value: i8 },
///   BestOf(
///     #[safelisp(field = "count")] u8,
///     #[safelisp(field = "dice")] Box<Dice>,
///   ),
/// }
/// ```
#[proc_macro_derive(SafelispValue, attributes(safelisp))]
pub fn derive_safelisp_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  expand(&input)
    .unwrap_or_else(syn::Error::into_compile_error)
    .into()
}

/// A field's name (or positional index) and Rust type, extracted uniformly
/// from named, tuple, and unit field lists.
struct Field {
  name: String,
  ty: Type,
}

/// Extract a `field = "name"` argument from `#[safelisp(field = "…")]` on a
/// single field. Returns `None` when the field carries no such attribute.
fn field_name_attr(attrs: &[Attribute]) -> Option<syn::Result<String>> {
  for attr in attrs {
    if !attr.path().is_ident("safelisp") {
      continue;
    }
    let list = match attr.meta.require_list() {
      Ok(list) => list,
      Err(err) => return Some(Err(err)),
    };
    let nv = match syn::parse2::<Meta>(list.tokens.clone()) {
      Ok(Meta::NameValue(nv)) => nv,
      _ => {
        return Some(Err(syn::Error::new_spanned(
          attr,
          "expected `field = \"name\"`",
        )))
      }
    };
    if !nv.path.is_ident("field") {
      return Some(Err(syn::Error::new_spanned(
        attr,
        "unsupported `#[safelisp]` field attribute; expected `field = \"name\"`",
      )));
    }
    if let Expr::Lit(ExprLit {
      lit: Lit::Str(s), ..
    }) = &nv.value
    {
      return Some(Ok(s.value()));
    }
    return Some(Err(syn::Error::new_spanned(
      &nv.value,
      "expected `field = \"name\"`",
    )));
  }
  None
}

/// Collect fields from a struct or a single enum variant. Positional (tuple)
/// fields must carry `#[safelisp(field = "name")]` to name them in Safelisp;
/// named fields use their own identifier and must not carry the attribute.
fn collect_fields(fields: &Fields) -> syn::Result<Vec<Field>> {
  match fields {
    Fields::Named(named) => named
      .named
      .iter()
      .map(|f| {
        if let Some(res) = field_name_attr(&f.attrs) {
          res?;
          return Err(syn::Error::new_spanned(
            f,
            "`#[safelisp(field = ...)]` is only valid on positional (tuple) fields",
          ));
        }
        let name = f.ident.as_ref().expect("named field").to_string();
        Ok(Field {
          name,
          ty: f.ty.clone(),
        })
      })
      .collect(),
    Fields::Unnamed(unnamed) => unnamed
      .unnamed
      .iter()
      .map(|f| {
        let name = match field_name_attr(&f.attrs) {
          Some(Ok(name)) => name,
          Some(Err(err)) => return Err(err),
          None => {
            return Err(syn::Error::new_spanned(
              f,
              "positional field requires `#[safelisp(field = \"name\")]` to name it in Safelisp",
            ))
          }
        };
        Ok(Field {
          name,
          ty: f.ty.clone(),
        })
      })
      .collect(),
    Fields::Unit => Ok(Vec::new()),
  }
}

/// Extract the `module = "..."` argument from `#[safelisp(module = "…")]`.
fn module_attr(input: &DeriveInput) -> syn::Result<String> {
  for attr in &input.attrs {
    if !attr.path().is_ident("safelisp") {
      continue;
    }
    let list = attr.meta.require_list()?;
    let nv = syn::parse2::<Meta>(list.tokens.clone())?;
    if let Meta::NameValue(nv) = nv {
      if nv.path.is_ident("module") {
        if let Expr::Lit(ExprLit {
          lit: Lit::Str(s), ..
        }) = &nv.value
        {
          return Ok(s.value());
        }
        return Err(syn::Error::new_spanned(
          &nv.value,
          "expected `module = \"name\"`",
        ));
      }
    }
    return Err(syn::Error::new_spanned(
      attr,
      "unsupported `#[safelisp]` attribute; expected `module = \"name\"`",
    ));
  }
  Err(syn::Error::new_spanned(
    &input.ident,
    "missing `#[safelisp(module = \"name\")]` attribute",
  ))
}

/// `fn sl_signature() -> Signature { Signature::named(module, name) }`.
fn signature_impl(module: &str, name: &Ident) -> TokenStream {
  quote! {
    fn sl_signature() -> ::safelisp::Signature {
      ::safelisp::Signature::named(#module, stringify!(#name))
    }
  }
}

/// Bindings `f0, f1, …` used for both `to_value` match arms and `from_value`
/// decoded locals.
fn field_bindings(n: usize) -> Vec<Ident> {
  (0..n).map(|i| format_ident!("f{i}")).collect()
}

/// `vec![ <Ty0>::to_value(b0, ctx)?, <Ty1>::to_value(b1, ctx)?, … ]`.
fn to_value_vec(fields: &[Field], bindings: &[Ident]) -> TokenStream {
  let exprs = fields.iter().zip(bindings).map(|(f, b)| {
    let ty = &f.ty;
    quote! { <#ty as ::safelisp::SafelispValue>::to_value(#b, ctx)? }
  });
  quote! { vec![#(#exprs),*] }
}

/// Statements that decode each positional field into a local `f{i}`:
/// `let f0 = fields.get(0).copied().ok_or(...)?; let f0 = <Ty>::from_value(ctx, f0)?;`
fn from_value_stmts(fields: &[Field], qualified: &str) -> TokenStream {
  let stmts = fields.iter().enumerate().map(|(i, f)| {
    let ty = &f.ty;
    let binding = format_ident!("f{i}");
    let err = format!("{qualified} missing field {i}");
    quote! {
      let #binding = fields.get(#i).copied().ok_or_else(|| #err.to_string())?;
      let #binding = <#ty as ::safelisp::SafelispValue>::from_value_with_depth(
        ctx,
        #binding,
        depth - 1,
      )?;
    }
  });
  quote! { #(#stmts)* }
}

/// `( vec![(name, <Ty>::sl_signature()), …] )` for `CustomTypeSpec`.
fn spec_fields(fields: &[Field]) -> TokenStream {
  let entries = fields.iter().map(|f| {
    let name = &f.name;
    let ty = &f.ty;
    quote! { (#name, <#ty as ::safelisp::SafelispValue>::sl_signature()) }
  });
  quote! { vec![#(#entries),*] }
}

/// Build a constructor expression for `Self` (struct) from decoded `f{i}`
/// locals: `Self { x: f0, y: f1 }`, `Self(f0, f1)`, or `Self`.
fn struct_ctor(fields: &Fields, bindings: &[Ident]) -> TokenStream {
  match fields {
    Fields::Named(named) => {
      let idents: Vec<Ident> = named
        .named
        .iter()
        .map(|f| f.ident.clone().expect("named field"))
        .collect();
      quote! { Self { #( #idents: #bindings ),* } }
    }
    Fields::Unnamed(_) => quote! { Self ( #( #bindings ),* ) },
    Fields::Unit => quote! { Self },
  }
}

/// Build a match pattern binding each field to `f{i}`.
fn fields_pattern(fields: &Fields, bindings: &[Ident]) -> TokenStream {
  match fields {
    Fields::Named(named) => {
      let idents: Vec<Ident> = named
        .named
        .iter()
        .map(|f| f.ident.clone().expect("named field"))
        .collect();
      quote! { Self { #( #idents : #bindings ),* } }
    }
    Fields::Unnamed(_) => quote! { Self ( #( #bindings ),* ) },
    Fields::Unit => quote! { Self },
  }
}

/// Build a variant constructor expression from decoded `f{i}` locals.
fn variant_ctor(v: &syn::Variant, bindings: &[Ident]) -> TokenStream {
  let vname = &v.ident;
  match &v.fields {
    Fields::Named(named) => {
      let idents: Vec<Ident> = named
        .named
        .iter()
        .map(|f| f.ident.clone().expect("named field"))
        .collect();
      quote! { #vname { #( #idents: #bindings ),* } }
    }
    Fields::Unnamed(_) => quote! { #vname ( #( #bindings ),* ) },
    Fields::Unit => quote! { #vname },
  }
}

/// Build a variant match pattern binding each field to `f{i}`.
fn variant_pattern(v: &syn::Variant, bindings: &[Ident]) -> TokenStream {
  let vname = &v.ident;
  match &v.fields {
    Fields::Named(named) => {
      let idents: Vec<Ident> = named
        .named
        .iter()
        .map(|f| f.ident.clone().expect("named field"))
        .collect();
      quote! { Self :: #vname { #( #idents : #bindings ),* } }
    }
    Fields::Unnamed(_) => quote! { Self :: #vname ( #( #bindings ),* ) },
    Fields::Unit => quote! { Self :: #vname },
  }
}

fn expand_struct(module: &str, name: &Ident, fields: &Fields) -> syn::Result<TokenStream> {
  let collected = collect_fields(fields)?;
  let bindings = field_bindings(collected.len());
  let sig = signature_impl(module, name);
  let pattern = fields_pattern(fields, &bindings);
  let to_vec = to_value_vec(&collected, &bindings);
  let ctor = struct_ctor(fields, &bindings);
  let decode = from_value_stmts(&collected, &format!("{module}::{name}"));
  let spec = spec_fields(&collected);

  Ok(quote! {
    impl ::safelisp::SafelispValue for #name {
      #sig
      fn to_value<'gc>(
        &self,
        ctx: &mut ::safelisp::HostCtx<'gc, '_>,
      ) -> Result<::safelisp::Value<'gc>, String> {
        match self {
          #pattern => {
            let __fields = #to_vec;
            ctx.alloc_struct(#module, stringify!(#name), __fields)
          }
        }
      }
      fn from_value_with_depth<'gc>(
        ctx: &::safelisp::HostCtx<'gc, '_>,
        value: ::safelisp::Value<'gc>,
        depth: usize,
      ) -> Result<Self, String> {
        if depth == 0 {
          return Err(format!(
            "{}::{} conversion depth limit exceeded",
            #module, stringify!(#name),
          ));
        }
        let instance = ctx.struct_instance(&value, #module, stringify!(#name))?;
        let fields = &instance.fields;
        #decode
        Ok(#ctor)
      }
    }
    impl ::safelisp::SafelispType for #name {
      fn type_spec() -> ::safelisp::CustomTypeSpec {
        ::safelisp::CustomTypeSpec::struct_(#module, stringify!(#name), #spec)
      }
    }
  })
}

fn expand_enum(
  module: &str,
  name: &Ident,
  variants: &syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
) -> syn::Result<TokenStream> {
  let sig = signature_impl(module, name);

  let collected: Vec<Vec<Field>> = variants
    .iter()
    .map(|v| collect_fields(&v.fields))
    .collect::<syn::Result<_>>()?;

  let to_arms = variants.iter().zip(&collected).map(|(v, collected)| {
    let vname = &v.ident;
    let bindings = field_bindings(collected.len());
    let pattern = variant_pattern(v, &bindings);
    let to_vec = to_value_vec(collected, &bindings);
    quote! {
      #pattern => {
        let __fields = #to_vec;
        ctx.alloc_enum(#module, stringify!(#name), stringify!(#vname), __fields)
      }
    }
  });

  let from_arms = variants
    .iter()
    .zip(&collected)
    .enumerate()
    .map(|(i, (v, collected))| {
      let vname = &v.ident;
      let bindings = field_bindings(collected.len());
      let qualified = format!("{module}::{name}::{vname}");
      let decode = from_value_stmts(collected, &qualified);
      let ctor = variant_ctor(v, &bindings);
      let idx = i as u16;
      quote! {
        #idx => {
          #decode
          Ok(Self::#ctor)
        },
      }
    });

  let spec_variants = variants.iter().zip(&collected).map(|(v, collected)| {
    let vname = &v.ident;
    let spec = spec_fields(collected);
    quote! { (stringify!(#vname), #spec) }
  });

  Ok(quote! {
    impl ::safelisp::SafelispValue for #name {
      #sig
      fn to_value<'gc>(
        &self,
        ctx: &mut ::safelisp::HostCtx<'gc, '_>,
      ) -> Result<::safelisp::Value<'gc>, String> {
        match self {
          #(#to_arms)*
        }
      }
      fn from_value_with_depth<'gc>(
        ctx: &::safelisp::HostCtx<'gc, '_>,
        value: ::safelisp::Value<'gc>,
        depth: usize,
      ) -> Result<Self, String> {
        if depth == 0 {
          return Err(format!(
            "{}::{} conversion depth limit exceeded",
            #module, stringify!(#name),
          ));
        }
        let instance = ctx.enum_instance(&value, #module, stringify!(#name))?;
        let fields = &instance.fields;
        match instance.variant {
          #(#from_arms)*
          n => Err(format!("invalid {}::{} variant index {}", #module, stringify!(#name), n)),
        }
      }
    }
    impl ::safelisp::SafelispType for #name {
      fn type_spec() -> ::safelisp::CustomTypeSpec {
        ::safelisp::CustomTypeSpec::enum_(#module, stringify!(#name), vec![#(#spec_variants),*])
      }
    }
  })
}

fn expand(input: &DeriveInput) -> syn::Result<TokenStream> {
  if !input.generics.params.is_empty() {
    return Err(syn::Error::new_spanned(
      &input.generics,
      "SafelispValue does not support generic types yet",
    ));
  }
  let module = module_attr(input)?;
  let name = &input.ident;
  match &input.data {
    Data::Struct(s) => expand_struct(&module, name, &s.fields),
    Data::Enum(e) => expand_enum(&module, name, &e.variants),
    Data::Union(u) => Err(syn::Error::new_spanned(
      u.union_token,
      "SafelispValue cannot be derived for unions",
    )),
  }
}
