# Stable Memory Tests

These test serve two purposes:
* Ensure backward compatibility with our stable memory format.
* Show that we can indeed fully recover in case of a disaster, assuming
  we have a backup of the stable memory.

## Creating New Test Memory Backups

### Using Canister Test Infrastructure
These backup memory files are generated using the `canister_tests` infrastructure. To create a new backup file:
1. Install II on the `StateMachine`
    ```rust
    let env = StateMachine::new();
    let canister_id = install_ii_canister(&env, framework::II_WASM.clone());
    ```
2. Create the desired state in II by interacting with it.
3. Export the stable memory using the helper function provided by the `framework`:
    ```rust
    framework::save_compressed_stable_memory(&env, canister_id, "stable_memory/stable.bin.gz", "stable_memory/stable.bin.gz");
    ```
### Using the Local Replica

The stable memory can also be copied from the local replica from the following path:
`~/Library/Application Support/org.dfinity.dfx/network/local/state/replicated_state/node-100/state/tip/canister_states/CANISTER_ID/stable_memory.bin`

## Registering the DFX Key as II Device

Registering the `dfx` public key in II can be useful for testing as certain calls (e.g. `register`) require the `caller` to match the submitted public keys.

To register dfx as a device:
1. Export the key in the DER file format:
    ```bash
    dfx identity export IDENTITY | openssl ec -out FILE_NAME -pubout -outform der
    ```
2. Make sure that the current `dfx` identity matches the one exported in step 1
    ```bash
    dfx identity use IDENTITY
    ```
3. Register an anchor using `dfx` (assuming DUMMY_CAPTCHA, otherwise the actual challenge key has to be submitted)
    ```bash
    challenge_key=$(dfx canister call II_CANISTER_ID create_challenge --candid src/internet_identity/internet_identity.did | sed -n 's/.*challenge_key[[:space:]]*=[[:space:]]*"\(.*\)".*/\1/p')
    pubkey_blob=$(hexdump -ve '1/1 "%.2x"' DER_PUBKEY_FILE_NAME | sed 's/../\\&/g')
    dfx canister call II_CANISTER_ID \
        register "(record{pubkey=blob \"$pubkey_blob\";alias=\"dfx test key\";purpose=variant{authentication};key_type=variant{unknown};protection=variant{unprotected};},record{key=\"$challenge_key\";chars=\"a\"})" \
        --candid src/internet_identity/internet_identity.did
    ```
**Note:** It is recommended to do this with an identity that is stored _unencrypted_. Otherwise, the passphrase has to be entered for every single `dfx` command.