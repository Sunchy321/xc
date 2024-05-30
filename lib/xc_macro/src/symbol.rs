use std::collections::HashMap;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::braced;
use syn::{punctuated::Punctuated, Token, Ident, LitStr};
use syn::parse::{Parse, ParseStream, Result};

mod kw {
    syn::custom_keyword!(Keywords);
    syn::custom_keyword!(Operators);
}

struct Keyword {
    name: Ident,
    value: LitStr,
}

impl Parse for Keyword {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let value = input.parse()?;
        Ok(Keyword { name, value })
    }
}

struct Operator {
    name: Ident,
    value: LitStr,
}

impl Parse for Operator {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let value = input.parse()?;
        Ok(Operator { name, value })
    }

}

struct Input {
    keywords: Punctuated<Keyword, Token![,]>,
    operators: Punctuated<Operator, Token![,]>
}

impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::Keywords>()?;
        let content;
        braced!(content in input);
        let keywords = Punctuated::parse_terminated(&content)?;

        input.parse::<kw::Operators>()?;
        let content;
        braced!(content in input);
        let operators = Punctuated::parse_terminated(&content)?;

        Ok(Input { keywords, operators })
    }
}

#[derive(Default)]
struct Errors {
    list: Vec<syn::Error>,
}

impl Errors {
    fn error(&mut self, span: Span, message: String) {
        self.list.push(syn::Error::new(span, message));
    }
}


struct Preinterned {
    idx: u32,
    span_of_name: Span,
}

struct Entries {
    map: HashMap<String, Preinterned>
}

impl Entries {
    fn with_capacity(cap: usize) -> Self {
        Self { map: HashMap::with_capacity(cap) }
    }

    fn insert(&mut self, span: Span, str: &str, errors: &mut Errors) -> u32 {
        if let Some(prev) = self.map.get(str) {
            errors.error(span, format!("Symbol `{str}` is duplicated"));
            errors.error(prev.span_of_name, "location of previous definition".to_string());
            prev.idx
        } else {
            let idx = self.len();
            self.map.insert(str.to_string(), Preinterned { idx, span_of_name: span });
            idx
        }
    }

    fn len(&self) -> u32 {
        u32::try_from(self.map.len()).expect("way too many symbols")
    }
}

fn parse_symbols_with_error(input: TokenStream) -> (TokenStream, Vec<syn::Error>) {
    let mut errors = Errors::default();

    let input: Input = match syn::parse2(input) {
        Ok(input) => input,
        Err(err) => {
            errors.list.push(err);
            Input { keywords: Default::default(), operators: Default::default() }
        }
    };

    let mut keyword_stream = quote! {};
    let mut operator_stream = quote! {};
    let mut prefill_stream = quote! {};
    let mut entries = Entries::with_capacity(input.keywords.len() + input.operators.len());

    for keyword in input.keywords.iter() {
        let name = &keyword.name;
        let value = &keyword.value;

        let idx = entries.insert(name.span(), &value.value(), &mut errors);

        prefill_stream.extend(quote! {
            #value,
        });
        keyword_stream.extend(quote! {
            pub const #name: Symbol = Symbol::new(#idx);
        });
    }

    for operator in input.operators.iter() {
        let name = &operator.name;
        let value = &operator.value;

        let idx = entries.insert(name.span(), &value.value(), &mut errors);

        prefill_stream.extend(quote! {
            #value,
        });
        operator_stream.extend(quote! {
            pub const #name: Symbol = Symbol::new(#idx);
        });
    }

    let output = quote! {
        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        pub(crate) mod kw_generated {
            use crate::Symbol;
            #keyword_stream
        }

        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        pub(crate) mod op_generated {
            use crate::Symbol;
            #operator_stream
        }

        impl Interner {
            pub(crate) fn fresh() -> Self {
                Interner::prefill(&[
                    #prefill_stream
                ])
            }
        }
    };

    (output, errors.list)
}

pub fn parse_symbols(input: TokenStream) -> TokenStream {
    let (mut output, errors) = parse_symbols_with_error(input);

    // If we generated any errors, then report them as compiler_error!() macro calls.
    // This lets the errors point back to the most relevant span. It also allows us
    // to report as many errors as we can during a single run.
    output.extend(errors.into_iter().map(|e| e.to_compile_error()));

    output
}