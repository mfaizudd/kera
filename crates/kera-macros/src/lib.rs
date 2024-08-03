use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

#[proc_macro_derive(TokenContainer)]
pub fn node_derive_macro(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let name = ast.ident;
    let expanded = quote! {
        impl TokenContainer for #name {
            fn token(&self) -> &Token {
                &self.token
            }
        }
    };
    expanded.into()
}
