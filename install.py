#!/usr/bin/env python3

import os
import sys
import library
import shutil


unexpanded_kaede_dir = "$HOME/.kaede"
unexpanded_kaede_bin_dir = os.path.join(unexpanded_kaede_dir, "bin")
kaede_dir = os.path.expandvars(unexpanded_kaede_dir)
kaede_bin_dir = os.path.expandvars(unexpanded_kaede_bin_dir)


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
    shutil.move("target/release/kaede", os.path.join(kaede_dir, "bin"))

    print("Done!")


def install():
    library.install(kaede_dir)
    install_compiler()


def create_shell_script_for_setting_env():
    import platform

    lib_extension = (
        "DYLD_LIBRARY_PATH" if platform.system() == "Darwin" else "LD_LIBRARY_PATH"
    )
    bdwgc_lib_path = os.path.join(unexpanded_kaede_dir, "third_party", "bdwgc", "lib")

    env_script_path = os.path.join(unexpanded_kaede_dir, "env")
    with open(os.path.expandvars(env_script_path), "w+") as f:
        f.writelines(
            [
                "#!/bin/sh\n",
                "\n",
                'export PATH="%s:$PATH"\n' % unexpanded_kaede_bin_dir,
                "\n",
                'export %s="%s:$%s"\n' % (lib_extension, bdwgc_lib_path, lib_extension),
            ]
        )

    if "--no-setenv" in sys.argv:
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
    if os.path.exists(kaede_dir):
        print(
            "Existing installation found at '%s'. Removing and reinstalling..."
            % kaede_dir
        )
        shutil.rmtree(kaede_dir)

    os.makedirs(kaede_bin_dir)

    install()

    create_shell_script_for_setting_env()
