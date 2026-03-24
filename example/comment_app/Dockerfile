FROM rust:1-bookworm AS builder

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    cmake \
    curl \
    dirmngr \
    gnupg \
    libssl-dev \
    pkg-config \
    python3 \
    lsb-release \
    wget \
    software-properties-common \
    gnupg \
 && rm -rf /var/lib/apt/lists/*

# Kaede currently targets LLVM 17.
RUN wget -O /tmp/llvm.sh https://apt.llvm.org/llvm.sh \
 && chmod +x /tmp/llvm.sh \
 && /tmp/llvm.sh 17 all \
 && rm /tmp/llvm.sh

ENV PATH="/usr/lib/llvm-17/bin:${PATH}"
ENV LLVM_SYS_170_PREFIX="/usr/lib/llvm-17"
ENV KAEDE_DIR="/root/.kaede"

RUN rustup toolchain install nightly --profile minimal

WORKDIR /app
COPY . .

RUN rm -rf library/bdwgc_build library/runtime_build

RUN ./install.py --no-setenv

WORKDIR /app/example/comment_app
RUN /root/.kaede/bin/kaede build

FROM debian:bookworm-slim AS runtime

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libgcc-s1 \
    libssl3 \
 && rm -rf /var/lib/apt/lists/*

ENV KAEDE_DIR="/root/.kaede"
ENV LD_LIBRARY_PATH="/root/.kaede/lib:/root/.kaede/third_party/bdwgc/lib"

WORKDIR /app/example/comment_app

COPY --from=builder /root/.kaede /root/.kaede
COPY --from=builder /app/example/comment_app /app/example/comment_app

EXPOSE 8080

# Render should use port 8080 for this service, because the current example
# binds to a fixed port in the Kaede source.
CMD ["./build/main"]
