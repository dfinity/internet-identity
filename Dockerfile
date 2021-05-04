# Use this with
#
# docker build --output out .
#
# and find the .wasmfile in out/

FROM ubuntu:20.10 AS build-state

ARG rust_version=1.45.2
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

# Install IC CDK optimizer
RUN curl --fail -sL https://download.dfinity.systems/cdk-rs/5807d2f7b523f630eddd69acd4b245a8b129eff9/ic-cdk-optimizer-linux-amd64.gz -o /opt/cargo/bin/ic-cdk-optimizer.gz && \
    gunzip /opt/cargo/bin/ic-cdk-optimizer.gz && \
    chmod 0755 /opt/cargo/bin/ic-cdk-optimizer

ENV CARGO_HOME=/cargo \
    CARGO_TARGET_DIR=/cargo_target \
    PATH=/cargo/bin:$PATH

COPY . .

ENV CANISTER_ID=rdmx6-jaaaa-aaaaa-aaadq-cai
RUN npm ci
RUN npm run build
RUN cargo build --target wasm32-unknown-unknown --release
RUN ic-cdk-optimizer /cargo_target/wasm32-unknown-unknown/release/idp_service.wasm -o /cargo_target/wasm32-unknown-unknown/release/idp_service.wasm
RUN cp /cargo_target/wasm32-unknown-unknown/release/idp_service.wasm .
RUN sha256sum idp_service.wasm
