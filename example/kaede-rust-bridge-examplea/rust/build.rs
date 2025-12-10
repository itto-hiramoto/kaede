fn main() {
    kaede_rust_bridge_codegen::generate("src/lib.rs", "../src").unwrap();
}
