use proc_macro2::TokenStream;
use synstructure::Structure;

pub(crate) struct DiagDerive<'a> {
    structure: Structure<'a>,
}

impl<'a> DiagDerive<'a> {
    pub(crate) fn new(structure: Structure<'a>) -> Self {
        Self { structure }
    }

    pub(crate) fn into_tokens(self) -> TokenStream {
        unimplemented!()
    }
}

