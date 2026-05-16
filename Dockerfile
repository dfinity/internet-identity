# Use this with
#
#  docker build -t internet-identity .
#  or use ./scripts/docker-build
#
# The docker image. To update, run `docker pull ubuntu` locally, and update the
# sha256:... accordingly.
FROM --platform=linux/amd64 ubuntu@sha256:bbf3d1baa208b7649d1d0264ef7d522e1dc0deeeaaf6085bf8e4618867f03494 as deps

ENV TZ=UTC

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && \
    apt -yq update && \
    apt -yqq install --no-install-recommends curl ca-certificates \
        build-essential pkg-config libssl-dev llvm-dev liblmdb-dev clang cmake jq

# Install node
RUN curl --fail -sSf https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
ENV NVM_DIR=/root/.nvm
COPY .node-version .node-version
RUN . "$NVM_DIR/nvm.sh" && nvm install "$(cat .node-version)"
RUN . "$NVM_DIR/nvm.sh" && nvm use "v$(cat .node-version)"
RUN . "$NVM_DIR/nvm.sh" && nvm alias default "v$(cat .node-version)"
RUN ln -s "$NVM_DIR/versions/node/v$(cat .node-version)" "$NVM_DIR/versions/node/default"
ENV PATH="$NVM_DIR/versions/node/default/bin/:${PATH}"
RUN node --version
RUN npm --version

# Install Rust and Cargo in /opt
ENV RUSTUP_HOME=/opt/rustup \
    CARGO_HOME=/cargo \
    PATH=/cargo/bin:$PATH

RUN mkdir -p ./scripts
COPY ./scripts/bootstrap ./scripts/bootstrap
COPY ./rust-toolchain.toml ./rust-toolchain.toml

RUN ./scripts/bootstrap
RUN curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
RUN wasm-pack --version

# Install sccache for caching rustc invocations across builds. Pinned for
# reproducibility — bump deliberately when upgrading.
ARG SCCACHE_VERSION=v0.15.0
RUN curl -sSfL "https://github.com/mozilla/sccache/releases/download/${SCCACHE_VERSION}/sccache-${SCCACHE_VERSION}-x86_64-unknown-linux-musl.tar.gz" \
        -o /tmp/sccache.tar.gz \
    && tar -xzf /tmp/sccache.tar.gz -C /tmp \
    && mv /tmp/sccache-${SCCACHE_VERSION}-x86_64-unknown-linux-musl/sccache /usr/local/bin/sccache \
    && chmod +x /usr/local/bin/sccache \
    && rm -rf /tmp/sccache* \
    && sccache --version

# Wrap rustc with sccache so unchanged compilation units are served from cache.
# Output bytes are content-addressed by source+flags+rustc, so the resulting
# wasm is bit-identical to a non-cached build.
ENV RUSTC_WRAPPER=sccache \
    SCCACHE_DIR=/sccache-cache

# Pre-build all cargo dependencies. Because cargo doesn't have a build option
# to build only the dependencies, we pretend that our project is a simple, empty
# `lib.rs`. When we COPY the actual files we make sure to `touch` lib.rs so
# that cargo knows to rebuild it with the new content.
COPY Cargo.lock .
COPY Cargo.toml .
COPY src/internet_identity/Cargo.toml src/internet_identity/Cargo.toml
COPY src/internet_identity_interface/Cargo.toml src/internet_identity_interface/Cargo.toml
COPY src/archive/Cargo.toml src/archive/Cargo.toml
COPY src/canister_tests/Cargo.toml src/canister_tests/Cargo.toml
COPY src/sig-verifier-js/Cargo.toml src/sig-verifier-js/Cargo.toml
COPY src/internet_identity_frontend/Cargo.toml src/internet_identity_frontend/Cargo.toml
COPY src/asset_util/Cargo.toml src/asset_util/Cargo.toml
ENV CARGO_TARGET_DIR=/cargo_target
COPY ./scripts/build ./scripts/build
RUN --mount=type=cache,target=/sccache-cache,id=ii-sccache,sharing=locked \
    mkdir -p src/internet_identity/src \
    && touch src/internet_identity/src/lib.rs \
    && mkdir -p src/internet_identity_interface/src \
    && touch src/internet_identity_interface/src/lib.rs \
    && mkdir -p src/archive/src \
    && touch src/archive/src/lib.rs \
    && mkdir -p src/canister_tests/src \
    && touch src/canister_tests/src/lib.rs \
    && mkdir -p src/sig-verifier-js/src \
    && touch src/sig-verifier-js/src/lib.rs \
    && mkdir -p src/asset_util/src \
    && touch src/asset_util/src/lib.rs \
    && mkdir -p src/internet_identity_frontend/src \
    && touch src/internet_identity_frontend/src/main.rs \
    && ./scripts/build --only-dependencies --internet-identity --archive \
    && sccache --show-stats \
    && rm -rf src

FROM deps as build_internet_identity

COPY . .

# The version baked in
ARG II_VERSION=

RUN touch src/*/src/lib.rs
RUN npm ci

RUN --mount=type=cache,target=/sccache-cache,id=ii-sccache,sharing=locked \
    ./scripts/build && sccache --show-stats
RUN sha256sum /internet_identity.wasm.gz

FROM deps as build_archive

COPY . .

RUN touch src/*/src/lib.rs

RUN --mount=type=cache,target=/sccache-cache,id=ii-sccache,sharing=locked \
    ./scripts/build --archive && sccache --show-stats
RUN sha256sum /archive.wasm.gz

FROM deps as build_internet_identity_frontend

COPY . .

# The version baked in
ARG II_VERSION=

RUN touch src/*/src/lib.rs
RUN npm ci

RUN --mount=type=cache,target=/sccache-cache,id=ii-sccache,sharing=locked \
    ./scripts/build --frontend && sccache --show-stats
RUN sha256sum /internet_identity_frontend.wasm.gz

FROM scratch AS scratch_internet_identity
COPY --from=build_internet_identity /internet_identity.wasm.gz /

FROM scratch AS scratch_archive
COPY --from=build_archive /archive.wasm.gz /

FROM scratch AS scratch_internet_identity_frontend
COPY --from=build_internet_identity_frontend /internet_identity_frontend.wasm.gz /
