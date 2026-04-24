{
  description = "Kaede programming language dev environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        llvm = pkgs.llvmPackages_17;
        libPathEnvVar =
          if pkgs.stdenv.isDarwin then "DYLD_LIBRARY_PATH" else "LD_LIBRARY_PATH";
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            llvm.llvm
            llvm.libllvm
            rustup
            cmake
            pkg-config
            openssl
            python3
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            valgrind
          ];

          shellHook = ''
            # Mirror install.py's env script: expose bdwgc's shared library
            # once `./install.py` has populated $HOME/.kaede.
            if [ -d "$HOME/.kaede/third_party/bdwgc/lib" ]; then
              export ${libPathEnvVar}="$HOME/.kaede/third_party/bdwgc/lib''${${libPathEnvVar}:+:''$${libPathEnvVar}}"
              export PATH="$HOME/.kaede/bin:$PATH"
            fi

            # Match CI: stable (with rustfmt+clippy) plus a minimal nightly for rustdoc.
            if command -v rustup >/dev/null; then
              rustup toolchain list | grep -q '^stable'  || rustup toolchain install stable --component rustfmt --component clippy
              rustup toolchain list | grep -q '^nightly' || rustup toolchain install nightly --profile minimal
            fi
          '';
        };
      });
}
