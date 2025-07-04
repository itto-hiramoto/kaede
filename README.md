[![test](https://github.com/itto-hiramoto/kaede/actions/workflows/test.yml/badge.svg)](https://github.com/itto-hiramoto/kaede/actions/workflows/test.yml)

> [!WARNING]
> As this project is still in the pre-release phase, there is still a possibility that the language specifications could change significantly!

# Installation

## Install dependencies with homebrew

### LLVM 17

```bash
$ brew install llvm@17
```

### Environment Variable Configuration

```bash
$ export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
```

If you want to make it permanent, put it in a shell initialization file.<br/>
For example, in the case of a `~/.profile`.

```bash
$ echo 'export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"' >> ~/.profile
```

## Install standard library and compiler

Make sure Python3 is installed.

```bash
$ ./install.py
$ kaede -h
```
