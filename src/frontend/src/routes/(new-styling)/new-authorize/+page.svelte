<script lang="ts">
  //This is the screen where you authenticate with a dapp
  //Signing in here gets you to a dapp
  //I called it authorize to mirror the existing nomenclature

  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import Button from "$lib/components/UI/Button.svelte";
  import PasskeyCard from "$lib/components/UI/PasskeyCard.svelte";
  import CenterCard from "$lib/components/UI/CenterCard.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import NameIdentityCard from "$lib/components/UI/NameIdentityCard.svelte";
  import FlyWrapper from "$lib/components/UI/animation/FlyWrapper.svelte";
  import BottomCardOrModal from "$lib/components/UI/BottomCardOrModal.svelte";
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

  let showPasskeyCard = $state(false);
  let showCreatingIdentity = $state(false);

  // TODO: this should really be pulled from a central store that initializes itself on load
  // TODO: of course we would also need to pull account/profile/role/login info, but one thing
  // TODO: after another
  let lastUsedIdentity = $state<LastUsed | undefined>();
  let authContext = $state.raw<AuthContext>();
  let dappName = $derived<string>(
    authContext ? authContext?.requestOrigin : "",
  );
  const connection = new Connection(readCanisterId(), readCanisterConfig());

  let onAuthenticate: (
    authenticatedConnection: AuthenticatedConnection,
  ) => void;

  const handleContinueWithPasskey = () => {
    showPasskeyCard = true;
  };

  const handleContinueWithGoogle = () => {
    //TODO
    console.log("continuing with google");
  };

  const handleConnectPasskey = async () => {
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

  const handleGotoCreateIdentity = () => {
    showCreatingIdentity = true;
  };

  const handleContinueWithLastUsedIdentity = () => {
    //TODO
    console.log("continuing with last used");
  };

  const handleContinueWithOtherIdentity = () => {
    lastUsedIdentity = undefined;
  };

  const handleCreateIdentity = async (name: string) => {
    const tempIdentity = await ECDSAKeyIdentity.generate({
      extractable: false,
    });
    await connection.identity_registration_start({ tempIdentity });
    const identity = await DiscoverablePasskeyIdentity.create({
      publicKey: {
        ...creationOptions([], undefined, undefined),
        user: {
          id: window.crypto.getRandomValues(new Uint8Array(16)),
          name: name,
          displayName: name,
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
      name: name,
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

  const close = () => {
    showPasskeyCard = false;
    showCreatingIdentity = false;
    hasTransitionedOut = false;
  };

  //TODO: there are be more elegant ways to do this
  let hasTransitionedOut = $state(false);

  const transitionedOut = () => {
    hasTransitionedOut = true;
  };
</script>

<!-- an element with data-page 'new-authorize-view' is necessary for the e2e tests to pass -->
<CenterContainer data-page="new-authorize-view">
  <CenterCard>
    <div class="flex flex-col gap-1">
      <h1 class="h1 font-bold">[Sign in]</h1>
      <p class="p font-medium">
        to continue with <span class="font-bold">{dappName}</span>
      </p>
    </div>
    {#if isNullish(lastUsedIdentity)}
      <Button
        onclick={handleContinueWithPasskey}
        class="w-full"
        variant="primary">Continue with Passkey</Button
      >
      <Button
        onclick={handleContinueWithGoogle}
        class="w-full"
        disabled
        variant="secondary">Continue with Google</Button
      >
    {:else}
      <!-- TODO: here we would actually select the account, not the identity -->
      <!-- TODO: text-left not working -->
      <Button
        onclick={handleContinueWithLastUsedIdentity}
        class="w-full px-6 py-4 text-left"
        variant="primary">Continue as {lastUsedIdentity.name}</Button
      >
      <Button
        onclick={handleContinueWithOtherIdentity}
        class="w-full px-6 py-4 text-left"
        variant="dashed">Use another Internet Identity</Button
      >
    {/if}
  </CenterCard>

  {#if showPasskeyCard}
    <BottomCardOrModal {close}>
      {#if !showCreatingIdentity}
        <FlyWrapper handleTransitionEnd={transitionedOut}>
          <PasskeyCard
            {handleConnectPasskey}
            {handleGotoCreateIdentity}
            {close}
          />
        </FlyWrapper>
      {:else if hasTransitionedOut}
        <!-- TODO: handle resizing -->
        <FlyWrapper delay={300}>
          <NameIdentityCard class="relative" {handleCreateIdentity} {close} />
        </FlyWrapper>
      {/if}
    </BottomCardOrModal>
  {/if}
</CenterContainer>
