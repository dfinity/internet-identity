<script lang="ts">
  import { isNullish, nonNullish } from "@dfinity/utils";
  import {
    AuthenticatedConnection,
    Connection,
    creationOptions,
  } from "$lib/utils/iiConnection";
  import { readCanisterId, readCanisterConfig } from "$lib/utils/init";
  import type { UserNumber } from "$lib/generated/internet_identity_types";
  import {
    CosePublicKey,
    DiscoverablePasskeyIdentity,
  } from "$lib/utils/discoverablePasskeyIdentity";
  import {
    type AuthContext,
    authenticationProtocol,
  } from "$lib/flows/authorize/postMessageInterface";
  import { ECDSAKeyIdentity } from "@dfinity/identity";
  import { inferPasskeyAlias, loadUAParser } from "$lib/flows/register";
  import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
  import { fetchDelegation } from "$lib/flows/authorize/fetchDelegation";

  interface LastUsed {
    number: UserNumber;
    name?: string;
  }

  const lastUsed: LastUsed = {
    number: BigInt(12345),
    name: "John Doe",
  };

  const connection = new Connection(readCanisterId(), readCanisterConfig());

  let authContext = $state.raw<AuthContext>();
  let continueAsIdentity = $state.raw<LastUsed | undefined>(lastUsed);
  let showContinueWithPasskey = $state.raw(false);
  let showCreatePasskey = $state.raw(false);
  let passkeyName = $state.raw("");

  let onAuthenticate: (
    authenticatedConnection: AuthenticatedConnection,
  ) => void;
  const onUseAnotherAccount = () => {
    continueAsIdentity = undefined;
  };
  const onContinueWithPasskey = () => {
    showContinueWithPasskey = true;
  };
  const onCancelContinueWithPasskey = () => {
    showContinueWithPasskey = false;
    showCreatePasskey = false;
  };
  const onCreatePasskey = () => {
    showCreatePasskey = true;
  };
  const onCancelCreatePasskey = () => {
    showCreatePasskey = false;
  };
  const onCreatePasskeyAndAuthenticate = async () => {
    const tempIdentity = await ECDSAKeyIdentity.generate({
      extractable: false,
    });
    await connection.identity_registration_start({ tempIdentity });
    const identity = await DiscoverablePasskeyIdentity.create({
      publicKey: {
        ...creationOptions([], undefined, undefined),
        user: {
          id: window.crypto.getRandomValues(new Uint8Array(16)),
          name: passkeyName,
          displayName: passkeyName,
        },
      },
    });
    const uaParser = loadUAParser();
    const alias = await inferPasskeyAlias({
      authenticatorType: identity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: identity.getAaguid(),
    });
    const deviceOrigin = window.location.origin;
    const result = await connection.identity_registration_finish({
      name: passkeyName,
      tempIdentity,
      identity,
      authnMethod: passkeyAuthnMethodData({
        alias,
        pubKey: identity.getPublicKey().toDer(),
        credentialId: identity.getCredentialId()!,
        authenticatorAttachment: identity.getAuthenticatorAttachment(),
        origin: deviceOrigin,
      }),
    });
    if (result.kind !== "loginSuccess") {
      throw new Error("Registration failed");
    }
    onAuthenticate(result.connection);
  };
  const onPickPasskeyAndAuthenticate = async () => {
    let userNumber: UserNumber;
    const passkeyIdentity = new DiscoverablePasskeyIdentity({
      credentialRequestOptions: {
        publicKey: creationOptions([], undefined, undefined),
      },
      getPublicKey: async (result) => {
        const lookupResult = await connection.lookupDeviceKey(
          new Uint8Array(result.rawId),
        );
        if (isNullish(lookupResult)) {
          throw new Error("Account not migrated yet");
        }
        userNumber = lookupResult.anchor_number;
        return CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey));
      },
    });
    const result = await connection.fromIdentity(
      () => userNumber,
      passkeyIdentity,
    );
    onAuthenticate(result.connection);
  };

  authenticationProtocol({
    authenticate: (context) => {
      authContext = context;
      return new Promise((resolve) => {
        onAuthenticate = async (authenticatedConnection) => {
          const derivationOrigin =
            context.authRequest.derivationOrigin ?? context.requestOrigin;
          const result = await fetchDelegation({
            connection: authenticatedConnection,
            derivationOrigin,
            publicKey: context.authRequest.sessionPublicKey,
            maxTimeToLive: context.authRequest.maxTimeToLive,
          });
          if ("error" in result) {
            return;
          }
          const [userKey, parsed_signed_delegation] = result;
          resolve({
            kind: "success",
            delegations: [parsed_signed_delegation],
            userPublicKey: new Uint8Array(userKey),
            authnMethod: "passkey",
          });
        };
      });
    },
    onProgress: () => {},
  });
</script>

{#snippet authenticate()}
  <div class="flex flex-col">
    <h1 class="h1">Sign in</h1>
    <p class="mb-4">To continue with dapp</p>
    {#if nonNullish(continueAsIdentity)}
      {@render continueAs(continueAsIdentity)}
    {:else}
      {@render pickAuthenticationMethod()}
    {/if}
  </div>
{/snippet}

{#snippet continueAs({ number, name }: LastUsed)}
  <div class="flex flex-col gap-1">
    <button>
      Continue as {name ?? number}
    </button>
    <button onclick={onUseAnotherAccount}>Use another account</button>
  </div>
{/snippet}

{#snippet pickAuthenticationMethod()}
  <div class="flex flex-col gap-1">
    <button onclick={onContinueWithPasskey}>Continue with Passkey</button>
    <button>Continue with Google</button>
  </div>
  {#if showContinueWithPasskey}
    {@render connectOrCreatePasskey()}
  {/if}
{/snippet}

{#snippet createPasskey()}
  <div class="flex w-52 flex-col gap-1 border">
    <div>Name your Passkey</div>
    <input placeholder="Your passkey name" bind:value={passkeyName} />
    <button
      onclick={onCreatePasskeyAndAuthenticate}
      disabled={passkeyName.length === 0}
    >
      Continue
    </button>
    <button onclick={onCancelCreatePasskey}>Back</button>
  </div>
{/snippet}

{#snippet connectOrCreatePasskey()}
  <button onclick={onCancelContinueWithPasskey}>X</button>
  {#if showCreatePasskey}
    {@render createPasskey()}
  {:else}
    <div class="flex flex-col gap-1">
      <button onclick={onPickPasskeyAndAuthenticate}>
        Connect my Passkey
      </button>
      <button onclick={onCreatePasskey}>
        Don't have a Passkey? Create one
      </button>
    </div>
  {/if}
{/snippet}

<div class="flex min-h-screen items-center justify-center">
  {#if nonNullish(authContext)}
    {@render authenticate()}
  {:else}
    <div>Loading...</div>
  {/if}
</div>
