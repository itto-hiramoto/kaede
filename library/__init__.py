#!/usr/bin/env python3

import pathlib
import shutil
import subprocess
import os
import tempfile

this_dir = os.path.dirname(os.path.abspath(__file__))


def install_bdwgc(third_party_dir):
    print("Installing garbage collector...")

    install_dir = os.path.join(third_party_dir, "bdwgc")

    bdwgc_dir = os.path.join(this_dir, "bdwgc")
    bdwgc_build_dir = os.path.join(this_dir, "bdwgc_build")

    def build_bdwgc():
        subprocess.run(["cmake", "-DCMAKE_BUILD_TYPE=Release", "-DCMAKE_INSTALL_PREFIX='%s'" %
                       install_dir, "-S", bdwgc_dir, "-B", bdwgc_build_dir]).check_returncode()
        subprocess.run(["cmake", "--build", bdwgc_build_dir, "-j"]).check_returncode()

    def install_bdwgc():
        build_bdwgc()
        subprocess.run(["cmake", "--install", bdwgc_build_dir]).check_returncode()

    install_bdwgc()

    print("Done!")

    return install_dir


def create_link_to_bdwgc(bdwgc_install_dir, dst):
    bdwgc_lib_path = os.path.join(bdwgc_install_dir, "lib", "libgc.so")

    # Create link to libgc.so
    os.symlink(bdwgc_lib_path, dst)


def install_standard_library(kaede_lib_dir):
    print("Installing standard library...")

    # Copy standard library source files
    kaede_lib_src_dir = os.path.join(kaede_lib_dir, "src")
    shutil.copytree(os.path.join(this_dir, "src"),
                    kaede_lib_src_dir)

    # Build standard library
    autoload_files = []
    std_lib_files = []
    for file in pathlib.Path(os.path.join(kaede_lib_src_dir, "autoload")).glob("**/*.kd"):
        autoload_files.append(str(file))
    for file in pathlib.Path(os.path.join(kaede_lib_src_dir, "std")).glob("**/*.kd"):
        std_lib_files.append(str(file))
    std_lib_files = [file for file in std_lib_files if file not in autoload_files]
    t1 = tempfile.NamedTemporaryFile()
    t2 = tempfile.NamedTemporaryFile()
    subprocess.run(["cargo", "run", "--release", "--", "--root-dir", os.path.join(kaede_lib_src_dir, "autoload"), "--no-autoload",
                   "-c", "-o", t1.name, *autoload_files]).check_returncode()
    subprocess.run(["cargo", "run", "--release", "--", "--root-dir", os.path.join(kaede_lib_src_dir, "std"), "--no-prelude",
                   "-c", "-o", t2.name, *std_lib_files]).check_returncode()
    subprocess.run(["gcc", "-shared", "-fPIC", "-o",
                   os.path.join(kaede_lib_dir, "libkd.so"), t1.name, t2.name]).check_returncode()

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

    install_standard_library(kaede_lib_dir)

    kaede_libgc_path = os.path.join(kaede_lib_dir, "libkgc.so")
    create_link_to_bdwgc(bdwgc_install_dir, kaede_libgc_path)
