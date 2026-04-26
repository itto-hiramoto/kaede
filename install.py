#!/usr/bin/env python3

import argparse
import os
import library
import shutil


def parse_args():
    parser = argparse.ArgumentParser(description="Install the Kaede toolchain.")
    parser.add_argument(
        "--prefix",
        default=None,
        help="Install directory (default: $HOME/.kaede). When set, the env "
        "activation script is not written and shell init files are not "
        "touched — the caller is expected to wire up the environment.",
    )
    parser.add_argument(
        "--no-setenv",
        action="store_true",
        help="Write the env activation script but don't modify shell init files.",
    )
    return parser.parse_args()


args = parse_args()

if args.prefix is not None:
    kaede_dir = os.path.abspath(args.prefix)
    unexpanded_kaede_dir = kaede_dir
else:
    unexpanded_kaede_dir = "$HOME/.kaede"
    kaede_dir = os.path.expandvars(unexpanded_kaede_dir)

unexpanded_kaede_bin_dir = os.path.join(unexpanded_kaede_dir, "bin")
kaede_bin_dir = os.path.join(kaede_dir, "bin")


def shell_initialize_file():
    shell_name = os.path.basename(os.environ.get("SHELL", ""))

    if shell_name == "bash":
        profile = "~/.bash_profile"
        login = "~/.bash_login"
    elif shell_name == "zsh":
        profile = "~/.zprofile"
        login = "~/.zlogin"
    else:
        return None

    if os.path.exists(os.path.expanduser(profile)):
        return profile

    if os.path.exists(os.path.expanduser(login)):
        return login

    return "~/.profile"


def install_compiler():
    print("Installing compiler...")

    # Binary was already built by `cargo run --release` invocations in library.install.
    shutil.move("target/release/kaede", os.path.join(kaede_bin_dir, "kaede"))

    print("Done!")


def install():
    library.install(kaede_dir)
    install_compiler()


def create_shell_script_for_setting_env():
    # When --prefix is set, the caller (e.g. a packaging system) is expected
    # to handle environment wiring on its own.
    if args.prefix is not None:
        return

    env_script_path = os.path.join(unexpanded_kaede_dir, "env")
    with open(os.path.expandvars(env_script_path), "w+") as f:
        f.writelines(
            [
                "#!/bin/sh\n",
                "\n",
                'export PATH="%s:$PATH"\n' % unexpanded_kaede_bin_dir,
            ]
        )

    if args.no_setenv:
        return

    shell_init_file = shell_initialize_file()

    if shell_init_file is None:
        shell_name = os.path.basename(os.environ.get("SHELL", "")) or "unknown"
        print(
            "Auto-config skipped: shell '%s' is not supported (only bash/zsh)."
            % shell_name
        )
        print("Please add the following to your shell init file:")
        print('  . "%s"' % env_script_path)
        return

    shell_init_file_path = os.path.expanduser(shell_init_file)
    line_to_add = '. "%s"\n' % env_script_path

    if os.path.exists(shell_init_file_path):
        with open(shell_init_file_path, "r") as f:
            if line_to_add in f.read():
                return

    with open(shell_init_file_path, "a+") as f:
        f.write(line_to_add)
    print("Please enter the following command:")
    print("source %s" % shell_init_file)


if __name__ == "__main__":
    # Auto-clean only the default location. When --prefix is supplied, the
    # caller owns the directory's lifecycle and may have already populated
    # it (e.g. a packaging system staging files under its build prefix).
    if args.prefix is None and os.path.exists(kaede_dir):
        print(
            "Existing installation found at '%s'. Removing and reinstalling..."
            % kaede_dir
        )
        shutil.rmtree(kaede_dir)

    os.makedirs(kaede_bin_dir, exist_ok=True)

    install()

    create_shell_script_for_setting_env()
