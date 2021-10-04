# Use this with
#
#  docker build -t internet-identity .
#  docker run --rm --entrypoint cat internet-identity /internet_identity.wasm > internet_identity.wasm
#
# and find the .wasmfile in out/

FROM ubuntu:20.10

ARG rust_version=1.51.0
ENV NODE_VERSION=14.15.4

ENV TZ=UTC

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && \
    apt -yq update && \
    apt -yqq install --no-install-recommends curl ca-certificates \
        build-essential pkg-config libssl-dev llvm-dev liblmdb-dev clang cmake

# Install node
RUN curl --fail -sSf https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash
ENV NVM_DIR=/root/.nvm
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
ENV PATH="/root/.nvm/versions/node/v${NODE_VERSION}/bin/:${PATH}"
RUN node --version
RUN npm --version

# Install Rust and Cargo in /opt
ENV RUSTUP_HOME=/opt/rustup \
    CARGO_HOME=/opt/cargo \
    PATH=/opt/cargo/bin:$PATH

RUN curl --fail https://sh.rustup.rs -sSf \
        | sh -s -- -y --default-toolchain ${rust_version}-x86_64-unknown-linux-gnu --no-modify-path && \
    rustup default ${rust_version}-x86_64-unknown-linux-gnu && \
    rustup target add wasm32-unknown-unknown

ENV CARGO_HOME=/cargo \
    CARGO_TARGET_DIR=/cargo_target \
    PATH=/cargo/bin:$PATH

# Install IC CDK optimizer
# (keep version in sync with src/internet_identity/build.sh)
RUN cargo install ic-cdk-optimizer --version 0.3.1

COPY . .

ENV CANISTER_ID=rdmx6-jaaaa-aaaaa-aaadq-cai
ARG II_ENV=production

RUN npm ci
RUN npm run build
RUN cargo build --target wasm32-unknown-unknown --release -j1
RUN sha256sum dist/*
RUN ic-cdk-optimizer /cargo_target/wasm32-unknown-unknown/release/internet_identity.wasm -o /cargo_target/wasm32-unknown-unknown/release/internet_identity.wasm
RUN cp /cargo_target/wasm32-unknown-unknown/release/internet_identity.wasm .
RUN sha256sum internet_identity.wasm
