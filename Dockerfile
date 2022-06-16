# Use this with
#
#  docker build -t internet-identity .
#  or use ./scripts/docker-build
#
# The docker image. To update, run `docker pull ubuntu` locally, and update the
# sha256:... accordingly.
FROM ubuntu@sha256:626ffe58f6e7566e00254b638eb7e0f3b11d4da9675088f4781a50ae288f3322 as deps

ARG rust_version=1.58.1
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
# (keep version in sync with scripts/build)
RUN cargo install ic-cdk-optimizer --version 0.3.1

COPY ./scripts ./scripts

# Pre-build all cargo dependencies. Because cargo doesn't have a build option
# to build only the dependecies, we pretend that our project is a simple, empty
# `lib.rs`. When we COPY the actual files we make sure to `touch` lib.rs so
# that cargo knows to rebuild it with the new content.
COPY Cargo.lock .
COPY Cargo.toml .
COPY src/internet_identity/Cargo.toml src/internet_identity/Cargo.toml
COPY src/internet_identity_interface/Cargo.toml src/internet_identity_interface/Cargo.toml
COPY src/canister_tests/Cargo.toml src/canister_tests/Cargo.toml
RUN mkdir -p src/internet_identity/src \
    && touch src/internet_identity/src/lib.rs \
    && mkdir -p src/internet_identity_interface/src \
    && touch src/internet_identity_interface/src/lib.rs \
    && mkdir -p src/canister_tests/src \
    && touch src/canister_tests/src/lib.rs \
    && ./scripts/build --only-dependencies \
    && rm -rf src

FROM deps as build

COPY . .

ARG II_FETCH_ROOT_KEY=
ARG II_DUMMY_CAPTCHA=
ARG II_DUMMY_AUTH=

RUN touch src/internet_identity/src/lib.rs
RUN touch src/internet_identity_interface/src/lib.rs
RUN touch src/canister_tests/src/lib.rs
RUN npm ci

RUN ./scripts/build
RUN sha256sum /internet_identity.wasm

FROM scratch AS scratch
COPY --from=build /internet_identity.wasm /
