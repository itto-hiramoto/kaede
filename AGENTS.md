# Repository Guidelines

## Project Structure & Module Organization
- Rust workspace in `Cargo.toml` with compiler crates in `compiler/` (lex/parse/ast/semantic/type_infer/codegen/driver/lsp/etc.).
- CLI entrypoint in `compiler/driver` (`kaede` binary); shared utilities in `compiler/common`, source locations (`span`), and symbol handling (`symbol`, `symbol_table`).
- Standard library lives in `library/src/std/`, C FFI implementations in `library/ffi/`; samples in `example/`.
- Build artifacts land in `target/`; keep working tree clean before commits.

## Build, Test, and Development Commands
- Install prerequisites once: `./install.py` (sets up standard library/runtime). WebSocket-enabled stdlib builds require `pkg-config` plus OpenSSL development files so `pkg-config --cflags --libs openssl` succeeds. On macOS add `export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"` to load LLVM.
- Optional: Nix users can run `nix develop` to enter a shell with LLVM 17, Rust, cmake, pkg-config, OpenSSL (and valgrind on Linux) preconfigured. `./install.py` still needs to be run once from inside the shell to populate `~/.kaede`.
- Format check: `cargo fmt --all -- --check`.
- Lint: `cargo clippy -- -D warnings`.
- Tests: `cargo test --release --no-fail-fast`.
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
- Keep tests deterministic; avoid parallel-only assumptions.

## Commit & Pull Request Guidelines
- Follow conventional commits seen in history (`feat:`, `fix:`, `refactor:`, `chore:`, etc.).
- Each PR should describe behavior changes, linked issues, and how to verify (commands run, sample input/output). Add screenshots only if UI output is relevant.
- If you change compiler behavior, language syntax, CLI usage, standard-library APIs, or examples that user-facing docs rely on, update the relevant docs in the same change set (`README.md`, `website/`, example docs, or other affected documentation) or explicitly state why no doc update was needed.
- If any Rust files are changed, run `cargo fmt --all` before committing.
- If any Rust files are changed, run `cargo clippy -- -D warnings` before committing.
- If formatting cannot be run or fails, report it explicitly and do not claim formatting was completed.
- If clippy cannot be run or reports warnings/errors, report it explicitly and do not claim lint checks passed.
- Ensure CI parity locally (`cargo fmt`, `cargo clippy`, `cargo test --release`); mention deviations or flaky areas in the PR.
- Prefer small, reviewable commits; keep generated artifacts (e.g., `target/`, build outputs) out of git.
