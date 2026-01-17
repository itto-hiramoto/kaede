# Repository Guidelines

## Project Structure & Module Organization
- Rust workspace in `Cargo.toml` with compiler crates in `compiler/` (lex/parse/ast/semantic/type_infer/codegen/driver/lsp/etc.).
- CLI entrypoint in `compiler/driver` (`kaede` binary); shared utilities in `compiler/common`, source locations (`span`), and symbol handling (`symbol`, `symbol_table`).
- Standard library lives in `library/src/std/`, C FFI implementations in `library/ffi/`, and Rust bridge codegen in `library/kaede-rust-bridge-codegen/`; samples in `example/`.
- Build artifacts land in `target/`; keep working tree clean before commits.

## Build, Test, and Development Commands
- Install prerequisites once: `./install.py` (sets up standard library/bridge code). On macOS add `export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"` to load LLVM.
- Format check: `cargo fmt --all -- --check`.
- Lint: `cargo clippy -- -D warnings`.
- Tests: `cargo test --release --no-fail-fast -- --test-threads=1` (CI uses single thread to avoid flakiness).
- Run the compiler locally: `cargo run -p kaede_compiler_driver -- <file.kaede> -o <out>`; add `--display-llvm-ir` or `-O3` as needed.
- Run the LSP server: `cargo run -p kaede_compiler_driver -- lsp` (stdio-based LSP).

## Coding Style & Naming Conventions
- Rust 2021 edition; default 4-space indentation.
- Prefer explicit modules per directory; keep public API narrow in non-driver crates.
- Use `cargo fmt` and `cargo clippy`; do not suppress warnings without justification.
- Naming: `UpperCamelCase` for types/traits, `snake_case` for functions/vars/files, `SCREAMING_SNAKE_CASE` for consts; align crate prefixes with `kaede_*`.

## Testing Guidelines
- Use Rust `#[test]` in-module or `tests/` integration suites; mirror issues with regression tests.
- For compiler features, include sample programs under `compiler/*/tests` or integration `tests/` that assert diagnostics/output.
- Keep tests deterministic; avoid parallel-only assumptions (CI runs `--test-threads=1`).

## Commit & Pull Request Guidelines
- Follow conventional commits seen in history (`feat:`, `fix:`, `refactor:`, `chore:`, etc.).
- Each PR should describe behavior changes, linked issues, and how to verify (commands run, sample input/output). Add screenshots only if UI output is relevant.
- Ensure CI parity locally (`cargo fmt`, `cargo clippy`, `cargo test --release`); mention deviations or flaky areas in the PR.
- Prefer small, reviewable commits; keep generated artifacts (e.g., `target/`, build outputs) out of git.
