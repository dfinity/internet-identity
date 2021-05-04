# Use this with
#
# docker build --output out .
#
# and fine the .wasmfile in out/

FROM ubuntu:20.04 AS build-state

ARG rust_version=1.45.2
ARG sdk_version=0.7.0-beta.5


ENV TZ=UTC

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && \
    apt -yq update && \
    apt -yqq install --no-install-recommends curl npm \
        build-essential pkg-config libssl-dev llvm-dev liblmdb-dev clang cmake

# Install Rust and Cargo in /opt
ENV RUSTUP_HOME=/opt/rustup \
    CARGO_HOME=/opt/cargo \
    PATH=/opt/cargo/bin:$PATH


RUN curl --fail https://sh.rustup.rs -sSf \
        | sh -s -- -y --default-toolchain ${rust_version}-x86_64-unknown-linux-gnu --no-modify-path && \
    rustup default ${rust_version}-x86_64-unknown-linux-gnu && \
    rustup target add wasm32-unknown-unknown

RUN curl --fail -L \
    "https://download.dfinity.systems/sdk/dfx/${sdk_version}/x86_64-linux/dfx-${sdk_version}.tar.gz" \
    -o /tmp/dfx.tar.gz && tar -xzf /tmp/dfx.tar.gz -C /usr/bin && chmod +x /usr/bin/dfx

# Install IC CDK optimizer
RUN curl --fail -sL https://download.dfinity.systems/cdk-rs/5807d2f7b523f630eddd69acd4b245a8b129eff9/ic-cdk-optimizer-linux-amd64.gz -o /opt/cargo/bin/ic-cdk-optimizer.gz && \
    gunzip /opt/cargo/bin/ic-cdk-optimizer.gz && \
    chmod 0755 /opt/cargo/bin/ic-cdk-optimizer

# dumb-init takes care to properly handle and forward signals as they are received (e.g. Ctrl+C or SIGSEGV)
# ENTRYPOINT with dumb-init is set further down in the Dockerfile
RUN curl --fail -L -o /usr/bin/dumb-init \
        https://github.com/Yelp/dumb-init/releases/download/v1.2.5/dumb-init_1.2.5_x86_64 && \
    chmod +x /usr/bin/dumb-init

ENV CARGO_HOME=/cargo \
    CARGO_TARGET_DIR=/cargo_target \
    PATH=/cargo/bin:$PATH

RUN pwd; find

COPY . .


ENV CANISTER_ID=rdmx6-jaaaa-aaaaa-aaadq-cai
RUN npm ci
RUN npm build
RUN cargo build --target wasm32-unknown-unknown --release
RUN ic-cdk-optimizer target/wasm32-unknown-unknown/release/idp_service.wasm target/wasm32-unknown-unknown/release/idp_service.wasm
RUN sha256sum target/wasm32-unknown-unknown/release/idp_service.wasm

FROM scratch AS export-stage
COPY --from=build-stage /target/wasm32-unknown-unknown/release/idp_service.wasm /
