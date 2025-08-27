# Ubuntu-based Kaede Programming Language development environment
FROM ubuntu:22.04

# Set non-interactive mode (prevents prompts during apt install)
ENV DEBIAN_FRONTEND=noninteractive

# Install basic dependencies
RUN apt-get update && apt-get install -y \
    # Basic tools
    curl \
    wget \
    git \
    build-essential \
    # Python 3
    python3 \
    python3-pip \
    # CMake (required for bdwgc build)
    cmake \
    # LLVM 17 dependencies
    software-properties-common \
    lsb-release \
    wget \
    gnupg \
    # valgrind (for testing, optional)
    valgrind \
    libzstd-dev \
    libssl-dev \
    pkg-config \
    neovim \
    && rm -rf /var/lib/apt/lists/*

# Install LLVM 17
RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 17 && \
    rm llvm.sh

# Install LLVM 17 dependencies
RUN apt-get update && apt-get install -y libpolly-17-dev && rm -rf /var/lib/apt/lists/*

# Set LLVM 17 environment variables
ENV LLVM_SYS_170_PREFIX=/usr/lib/llvm-17
ENV PATH="/usr/lib/llvm-17/bin:${PATH}"

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Add Rust components
RUN rustup component add rustfmt clippy

# Set working directory
WORKDIR /kaede

# Copy project files including Git submodules
COPY . .

# Change Git submodule URL from SSH to HTTPS (no SSH keys in Docker)
RUN git config --file .gitmodules submodule.library/bdwgc.url https://github.com/ivmai/bdwgc.git && \
    git submodule sync && \
    git submodule update --init --recursive

# Run Kaede installation
RUN chmod +x install.py && ./install.py --no-setenv

# Set Kaede environment variables
ENV KAEDE_DIR=/root/.kaede
ENV PATH="/root/.kaede/bin:${PATH}"
ENV LD_LIBRARY_PATH="/root/.kaede/third_party/bdwgc/lib:/root/.kaede/lib:${LD_LIBRARY_PATH}"

# Default command
CMD ["/bin/bash"]

# Health check command
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD kaede --help || exit 1

# Volume mount point (for development)
VOLUME ["/workspace"]

# Port (if needed)
# EXPOSE 8000

# Metadata
LABEL maintainer="kaede-dev"
LABEL description="Kaede Programming Language Development Environment"
LABEL version="1.0"
