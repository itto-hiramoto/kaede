{
  description = "Kaede programming language dev environment";

  inputs = {
    # Pinned to nixos-24.11: still carries llvmPackages_17, which CI pins via
    # KyleMayes/install-llvm-action. Newer nixpkgs channels have dropped
    # llvmPackages_17 as unmaintained.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        llvm = pkgs.llvmPackages_17;
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            rustup
            cmake
            pkg-config
            python3
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            valgrind
          ];

          buildInputs = with pkgs; [
            llvm.llvm
            llvm.libllvm
            libffi
            libxml2
            openssl
          ];

          shellHook = ''
            # Defer LD_LIBRARY_PATH / DYLD_LIBRARY_PATH / PATH setup to
            # install.py's env script — it is the single source of truth
            # for where the installed bdwgc and kaede binaries live.
            [ -f "$HOME/.kaede/env" ] && . "$HOME/.kaede/env"

            # Match CI: stable (with rustfmt+clippy) plus a minimal nightly for rustdoc.
            if command -v rustup >/dev/null; then
              rustup toolchain list | grep -q '^stable'  || rustup toolchain install stable --component rustfmt --component clippy
              rustup toolchain list | grep -q '^nightly' || rustup toolchain install nightly --profile minimal
            fi
          '';
        };
      });
}
