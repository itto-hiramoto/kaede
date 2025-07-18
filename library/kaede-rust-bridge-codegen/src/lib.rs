use proc_macro2::TokenStream;
use quote::quote;
use std::fs::OpenOptions;
use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
};
use syn::{parse_file, File};

pub fn generate(input_path: &str, kaede_out_dir: &str) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(input_path)?;
    let ast: File = parse_file(&content)?;

    let out_path = create_kaede_decls_file_path(kaede_out_dir);
    if out_path.exists() {
        fs::remove_file(out_path)?;
    }

    let mut tokens = TokenStream::new();
    for item in ast.items {
        if let syn::Item::Fn(func) = item {
            if matches!(func.vis, syn::Visibility::Public(_)) {
                let sig = &func.sig;
                let name = &sig.ident;
                let inputs = &sig.inputs;
                let output = &sig.output;

                // Extract parameter names for the function call
                let param_names: Vec<_> = sig
                    .inputs
                    .iter()
                    .filter_map(|arg| {
                        if let syn::FnArg::Typed(typed) = arg {
                            Some(&typed.pat)
                        } else {
                            None
                        }
                    })
                    .collect();

                let mangled_name =
                    syn::Ident::new(&format!("kaede_rust_bridge_{name}"), name.span());

                tokens.extend(quote! {
                    #[unsafe(no_mangle)]
                    pub extern "C" fn #mangled_name(#inputs) #output {
                        crate::#name(#(#param_names),*)
                    }
                });

                generate_kaede_decls(func, kaede_out_dir)?;
            }
        }
    }

    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR is not set");

    let out_path = Path::new(&out_dir).join("kaede_bindings.rs");
    let mut file = fs::File::create(out_path)?;
    write!(file, "{tokens}")?;
    Ok(())
}

fn create_kaede_decls_file_path(kaede_out_dir: &str) -> PathBuf {
    PathBuf::from(kaede_out_dir).join("krb_generated.kd")
}

fn generate_kaede_decls(
    func: syn::ItemFn,
    kaede_out_dir: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    assert!(matches!(func.vis, syn::Visibility::Public(_)));

    let sig = &func.sig;
    let name = &sig.ident;
    let inputs = &sig.inputs;
    let output = &sig.output;

    let out_path = create_kaede_decls_file_path(kaede_out_dir);
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(out_path)
        .unwrap();

    let output = match output {
        syn::ReturnType::Default => "".to_string(),
        syn::ReturnType::Type(_, ty) => format!(": {}", quote! { #ty }),
    };

    writeln!(
        file,
        "{}",
        format!(
            r#"pub bridge "Rust" fn {}({}){}"#,
            quote! { #name },
            quote! { #inputs },
            output
        )
    )?;

    Ok(())
}
