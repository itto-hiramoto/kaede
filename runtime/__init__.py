import os
import pathlib
import shutil

this_dir = os.path.dirname(os.path.abspath(__file__))


def install_runtime_sources(destination, exclude_core=False):
    """
    Copy runtime sources to destination, replacing any existing directory.
    If exclude_core is True, skip the core/ directory.
    """
    if os.path.exists(destination):
        shutil.rmtree(destination)

    def ignore_patterns(current_dir, names):
        ignored = set(["__pycache__"])
        if exclude_core and os.path.abspath(current_dir) == this_dir and "core" in names:
            ignored.add("core")
        return ignored

    shutil.copytree(this_dir, destination, ignore=ignore_patterns, dirs_exist_ok=True)
    return destination


def collect_runtime_c_sources(runtime_dir):
    c_files = []
    for path in pathlib.Path(runtime_dir).glob("**/*.c"):
        if "core" in path.parts:
            continue
        c_files.append(str(path))
    return c_files
