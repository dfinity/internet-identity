<script lang="ts">
  import { inferPasskeyAlias, loadUAParser } from "$lib/flows/register";
  import {
    AuthenticatedConnection,
    Connection,
    creationOptions,
  } from "$lib/utils/iiConnection";
  import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
  import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
  import {
    DelegationChain,
    DelegationIdentity,
    ECDSAKeyIdentity,
  } from "@dfinity/identity";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import Flow from "$lib/components/Flow.svelte";
  import { renderManage } from "$lib/flows/manage";
  import { PreLoadImage } from "$lib/utils/preLoadImage";
  import identityCardBackground from "$lib/legacy/assets/identityCardBackground.png?url";
  import {
    CosePublicKey,
    DiscoverablePasskeyIdentity,
  } from "$lib/utils/discoverablePasskeyIdentity";
  import type { UserNumber } from "$lib/generated/internet_identity_types";

  let authenticatedConnection = $state.raw<AuthenticatedConnection>();

  const connection = new Connection(readCanisterId(), readCanisterConfig());

  const registerWithPasskey = async () => {
    const tempIdentity = await ECDSAKeyIdentity.generate({
      extractable: false,
    });
    await connection.identity_registration_start({ tempIdentity });
    const rpId =
      origin === window.location.origin ? undefined : new URL(origin).hostname;
    const identity = await DiscoverablePasskeyIdentity.create({
      publicKey: creationOptions([], undefined, rpId),
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
      name: prompt("Account name?") ?? undefined,
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
    authenticatedConnection = result.connection;
  };
  const continueWithPasskey = async () => {
    let anchorNumber: UserNumber;
    const rpId =
      origin === window.location.origin ? undefined : new URL(origin).hostname;
    const passkeyIdentity = new DiscoverablePasskeyIdentity({
      credentialRequestOptions: {
        publicKey: creationOptions([], undefined, rpId),
      },
      getPublicKey: async (result) => {
        const lookupResult = await connection.getPubKeyByCredentialId(
          result.rawId,
        );
        if (isNullish(lookupResult)) {
          throw new Error("Nope");
        }
        anchorNumber = lookupResult.anchor_number;
        return CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey));
      },
    });
    const result = await connection.fromIdentity(
      () => anchorNumber,
      passkeyIdentity,
    );
    authenticatedConnection = result.connection;
  };
  const continueWithGoogle = () => {};
</script>

{#if nonNullish(authenticatedConnection)}
  <Flow
    promise={renderManage}
    args={[
      {
        connection: authenticatedConnection,
        userNumber: authenticatedConnection.userNumber,
        identityBackground: new PreLoadImage(identityCardBackground),
      },
    ]}
  />
{:else}
  <button onclick={registerWithPasskey}>Register with Passkey</button><br />
  <button onclick={continueWithPasskey}>Continue with Passkey</button><br />
  <button onclick={continueWithGoogle}>Continue with Google</button><br />
{/if}
