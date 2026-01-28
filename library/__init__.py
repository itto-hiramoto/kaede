#!/usr/bin/env python3

import pathlib
import shutil
import subprocess
import os
import tempfile
import platform

this_dir = os.path.dirname(os.path.abspath(__file__))


def install_bdwgc(third_party_dir):
    print("Installing garbage collector...")

    install_dir = os.path.join(third_party_dir, "bdwgc")

    bdwgc_dir = os.path.join(this_dir, "bdwgc")
    bdwgc_build_dir = os.path.join(this_dir, "bdwgc_build")

    def build_bdwgc():
        subprocess.run(
            [
                "cmake",
                "-DCMAKE_BUILD_TYPE=Release",
                "-DCMAKE_INSTALL_PREFIX='%s'" % install_dir,
                "-S",
                bdwgc_dir,
                "-B",
                bdwgc_build_dir,
            ]
        ).check_returncode()
        subprocess.run(["cmake", "--build", bdwgc_build_dir, "-j"]).check_returncode()

    def install_bdwgc():
        build_bdwgc()
        subprocess.run(["cmake", "--install", bdwgc_build_dir]).check_returncode()

    install_bdwgc()

    print("Done!")

    return install_dir


def install_kaede_rust_bridge_codegen(kaede_dir):
    print("Installing kaede-rust-bridge-codegen...")

    codegen_src_dir = os.path.join(this_dir, "kaede-rust-bridge-codegen")
    codegen_dst_dir = os.path.join(kaede_dir, "kaede-rust-bridge-codegen")

    # Copy the codegen crate
    if os.path.exists(codegen_dst_dir):
        shutil.rmtree(codegen_dst_dir)
    shutil.copytree(codegen_src_dir, codegen_dst_dir)

    print("Done!")


def install_standard_library(kaede_lib_dir, bdwgc_lib_path, bdwgc_include_dir, kaede_lib_path):
    print("Installing standard library...")

    # Copy standard library source files
    kaede_lib_src_dir = os.path.join(kaede_lib_dir, "src")
    shutil.copytree(os.path.join(this_dir, "src"), kaede_lib_src_dir)
    kaede_ffi_src_dir = os.path.join(kaede_lib_dir, "ffi")
    ffi_src_dir = os.path.join(this_dir, "ffi")
    if os.path.exists(ffi_src_dir):
        shutil.copytree(ffi_src_dir, kaede_ffi_src_dir)

    # Build standard library
    autoload_files = []
    std_lib_files = []
    ffi_c_files = []
    for path in pathlib.Path(kaede_ffi_src_dir).glob("**/*.c"):
        ffi_c_files.append(str(path))
    for file in pathlib.Path(os.path.join(kaede_lib_src_dir, "autoload")).glob(
        "**/*.kd"
    ):
        autoload_files.append(str(file))
    for file in pathlib.Path(os.path.join(kaede_lib_src_dir, "std")).glob("**/*.kd"):
        std_lib_files.append(str(file))
    std_lib_files = [file for file in std_lib_files if file not in autoload_files]
    t1 = tempfile.NamedTemporaryFile()
    t2 = tempfile.NamedTemporaryFile()
    subprocess.run(
        [
            "cargo",
            "run",
            "--release",
            "--",
            "--root-dir",
            os.path.join(kaede_lib_src_dir, "autoload"),
            "--no-autoload",
            "--no-prelude",
            "-c",
            "-o",
            t1.name,
            *autoload_files,
        ]
    ).check_returncode()
    subprocess.run(
        [
            "cargo",
            "run",
            "--release",
            "--",
            "--root-dir",
            os.path.join(kaede_lib_src_dir),
            "--no-prelude",
            "-c",
            "-o",
            t2.name,
            *std_lib_files,
        ]
    ).check_returncode()
    subprocess.run(
        [
            "gcc",
            "-shared",
            "-fPIC",
            "-I",
            bdwgc_include_dir,
            "-o",
            kaede_lib_path,
            t1.name,
            t2.name,
            *ffi_c_files,
            bdwgc_lib_path,
        ]
    ).check_returncode()

    print("Done!")


def install_runtime(kaede_lib_dir):
    print("Installing runtime library...")

    runtime_src_dir = os.path.join(this_dir, "runtime")
    runtime_build_dir = os.path.join(this_dir, "runtime_build")
    runtime_lib_path = os.path.join(kaede_lib_dir, "libkaede_runtime.a")

    if not os.path.exists(runtime_build_dir):
        os.mkdir(runtime_build_dir)

    subprocess.run(
        [
            "cmake",
            "-DCMAKE_BUILD_TYPE=Release",
            "-S",
            runtime_src_dir,
            "-B",
            runtime_build_dir,
        ]
    ).check_returncode()
    subprocess.run(["cmake", "--build", runtime_build_dir, "-j"]).check_returncode()

    shutil.copy(os.path.join(runtime_build_dir, "libkaede_runtime.a"), runtime_lib_path)

    print("Done!")


# Install libraries
def install(kaede_dir):
    third_party_dir = os.path.join(kaede_dir, "third_party")
    if not os.path.exists(third_party_dir):
        os.mkdir(third_party_dir)

    bdwgc_install_dir = install_bdwgc(third_party_dir)

    kaede_lib_dir = os.path.join(kaede_dir, "lib")
    if not os.path.exists(kaede_lib_dir):
        os.mkdir(kaede_lib_dir)

    # On macOS, use .dylib; on Linux, use .so
    lib_extension = "dylib" if platform.system() == "Darwin" else "so"
    bdwgc_lib_path = os.path.join(bdwgc_install_dir, "lib", f"libgc.{lib_extension}")
    kaede_lib_path = os.path.join(kaede_lib_dir, f"libkd.{lib_extension}")

    install_standard_library(
        kaede_lib_dir,
        bdwgc_lib_path,
        os.path.join(bdwgc_install_dir, "include"),
        kaede_lib_path,
    )
    install_runtime(kaede_lib_dir)
    install_kaede_rust_bridge_codegen(kaede_dir)

    # Create a symbolic link to easily link with GC from compiler side
    kaede_libgc_path = os.path.join(kaede_lib_dir, f"libkgc.{lib_extension}")
    os.symlink(bdwgc_lib_path, kaede_libgc_path)
