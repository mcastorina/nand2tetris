use proc_macro::TokenStream;

#[proc_macro]
pub fn Tpp(item: TokenStream) -> TokenStream {
    "fn answer() -> u32 { 42 }".parse().unwrap()
}
