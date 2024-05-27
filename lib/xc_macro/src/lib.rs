use proc_macro::TokenStream;

mod symbol;
mod diag;

#[proc_macro]
pub fn define_symbols(input: TokenStream) -> TokenStream {
    symbol::parse_symbols(input.into()).into()
}