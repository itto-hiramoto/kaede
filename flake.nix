{
  description = "Kaede programming language dev environment";

  inputs = {
    # nixos-24.11 is the last channel that still ships llvmPackages_17,
    # which CI pins via KyleMayes/install-llvm-action.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        llvm = pkgs.llvmPackages_17;

        # Runtime libraries the kaede binary loads dynamically:
        # libLLVM pulls in libffi/libxml2/ncurses/zlib and the binary
        # itself needs libstdc++ from gcc.
        kaedeRuntimeLibs = with pkgs; [
          llvm.libllvm
          libffi
          libxml2
          ncurses
          zlib
          stdenv.cc.cc.lib
        ];
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
            # install.py's env script owns LD_LIBRARY_PATH / DYLD_LIBRARY_PATH / PATH
            # for the installed bdwgc and kaede binaries.
            [ -f "$HOME/.kaede/env" ] && . "$HOME/.kaede/env"

            # mkShell injects -L but not -rpath for buildInputs, so a binary built
            # here would link cleanly yet fail at startup looking for libffi.so.8
            # etc. Bake an explicit RPATH so the kaede binary is self-contained
            # and works outside the dev shell.
            export RUSTFLAGS="-C link-arg=-Wl,-rpath,${pkgs.lib.makeLibraryPath kaedeRuntimeLibs} ''${RUSTFLAGS:-}"

            # Mirror CI's toolchain layout (stable + minimal nightly for rustdoc).
            if command -v rustup >/dev/null; then
              rustup toolchain list | grep -q '^stable'  || rustup toolchain install stable --component rustfmt --component clippy
              # Pinned nightly: rustdoc JSON format_version must match the rustdoc-types
              # crate version used by compiler/semantic. Bump together when upgrading.
              rustup toolchain list | grep -q '^nightly-2026-03-17' || rustup toolchain install nightly-2026-03-17 --profile minimal
            fi
          '';
        };
      });
}
