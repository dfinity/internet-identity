FROM ubuntu:latest

COPY dfx.json .
RUN apt -yq update && apt -yqq install --no-install-recommends curl  ca-certificates
# install dfx
ENV DFX_VERSION=0.10.1
RUN sh -ci "$(curl -fsSL https://sdk.dfinity.org/install.sh)"

ARG dfx_emulator_flag
ENTRYPOINT dfx start --clean --background $dfx_emulator_flag && dfx deploy && tail -f /dev/null
