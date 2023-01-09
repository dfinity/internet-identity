# Use this with
#
#  docker build -t internet-identity .
#  or use ./scripts/docker-build
#
# The docker image. To update, run `docker pull ubuntu` locally, and update the
# sha256:... accordingly.
FROM ubuntu@sha256:626ffe58f6e7566e00254b638eb7e0f3b11d4da9675088f4781a50ae288f3322 as deps

ENV TZ=UTC

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && \
    apt -yq update && \
    apt -yqq install --no-install-recommends curl ca-certificates \
        build-essential pkg-config libssl-dev llvm-dev liblmdb-dev clang cmake

# Install node
RUN curl --fail -sSf https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash
ENV NVM_DIR=/root/.nvm
COPY .node-version .node-version
RUN . "$NVM_DIR/nvm.sh" && nvm install "$(cat .node-version)"
RUN . "$NVM_DIR/nvm.sh" && nvm use "v$(cat .node-version)"
RUN . "$NVM_DIR/nvm.sh" && nvm alias default "v$(cat .node-version)"
RUN ln -s "/root/.nvm/versions/node/v$(cat .node-version)" /root/.nvm/versions/node/default
ENV PATH="/root/.nvm/versions/node/default/bin/:${PATH}"
RUN node --version
RUN npm --version

# Install Rust and Cargo in /opt
ENV RUSTUP_HOME=/opt/rustup \
    CARGO_HOME=/cargo \
    PATH=/cargo/bin:$PATH

RUN mkdir -p ./scripts
COPY ./scripts/bootstrap ./scripts/bootstrap
COPY ./rust-toolchain.toml ./rust-toolchain.toml

# bootstrap needs ~/.local/bin on PATH
ENV PATH="/root/.local/bin:${PATH}"
RUN ./scripts/bootstrap

# Pre-build all cargo dependencies. Because cargo doesn't have a build option
# to build only the dependecies, we pretend that our project is a simple, empty
# `lib.rs`. When we COPY the actual files we make sure to `touch` lib.rs so
# that cargo knows to rebuild it with the new content.
COPY Cargo.lock .
COPY Cargo.toml .
COPY src/internet_identity/Cargo.toml src/internet_identity/Cargo.toml
COPY src/internet_identity_interface/Cargo.toml src/internet_identity_interface/Cargo.toml
COPY src/archive/Cargo.toml src/archive/Cargo.toml
COPY src/canister_tests/Cargo.toml src/canister_tests/Cargo.toml
COPY src/state_machine_client/Cargo.toml src/state_machine_client/Cargo.toml
ENV CARGO_TARGET_DIR=/cargo_target
COPY ./scripts/build ./scripts/build
RUN mkdir -p src/internet_identity/src \
    && touch src/internet_identity/src/lib.rs \
    && mkdir -p src/internet_identity_interface/src \
    && touch src/internet_identity_interface/src/lib.rs \
    && mkdir -p src/archive/src \
    && touch src/archive/src/lib.rs \
    && mkdir -p src/canister_tests/src \
    && touch src/canister_tests/src/lib.rs \
    && mkdir -p src/state_machine_client/src \
    && touch src/state_machine_client/src/lib.rs \
    && ./scripts/build --only-dependencies \
    && rm -rf src

FROM deps as build_internet_identity

COPY . .

# The version baked in
ARG II_VERSION=

# The features, see README
ARG II_FETCH_ROOT_KEY=
ARG II_DUMMY_CAPTCHA=
ARG II_DUMMY_AUTH=
ARG II_INSECURE_REQUESTS=

RUN touch src/internet_identity/src/lib.rs
RUN touch src/internet_identity_interface/src/lib.rs
RUN touch src/canister_tests/src/lib.rs
RUN touch src/state_machine_client/src/lib.rs
RUN npm ci

RUN ./scripts/build
RUN sha256sum /internet_identity.wasm

FROM deps as build_archive

COPY . .

RUN touch src/internet_identity_interface/src/lib.rs
RUN touch src/archive/src/lib.rs
RUN touch src/canister_tests/src/lib.rs
RUN touch src/state_machine_client/src/lib.rs

RUN ./scripts/build --archive
RUN sha256sum /archive.wasm

FROM scratch AS scratch_internet_identity
COPY --from=build_internet_identity /internet_identity.wasm /

FROM scratch AS scratch_archive
COPY --from=build_archive /archive.wasm /
