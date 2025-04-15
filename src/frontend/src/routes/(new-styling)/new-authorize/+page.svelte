<script lang="ts">
  //This is the screen where you authenticate with a dapp
  //Signing in here gets you to a dapp
  //I called it authorize to mirror the existing nomenclature

  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import Button from "$lib/components/UI/Button.svelte";
  import CenterCard from "$lib/components/UI/CenterCard.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import BottomCardOrModal from "$lib/components/UI/BottomCardOrModal.svelte";
  import {
    AuthenticatedConnection,
    Connection,
    creationOptions,
  } from "$lib/utils/iiConnection";
  import { readCanisterId, readCanisterConfig } from "$lib/utils/init";
  import type {
    CheckCaptchaError,
    IdRegFinishError,
    IdRegStartError,
    OpenIdDelegationError,
    UserNumber,
  } from "$lib/generated/internet_identity_types";
  import {
    CosePublicKey,
    DiscoverablePasskeyIdentity,
  } from "$lib/utils/discoverablePasskeyIdentity";
  import {
    type AuthContext,
    authenticationProtocol,
  } from "$lib/flows/authorize/postMessageInterface";
  import { inferPasskeyAlias, loadUAParser } from "$lib/flows/register";
  import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
  import { fetchDelegation } from "$lib/flows/authorize/fetchDelegation";
  import type { PageProps } from "./$types";
  import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
  import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
  import { authenticateWithJWT } from "$lib/utils/authenticate/jwt";
  import { fly } from "svelte/transition";

  interface LastUsed {
    number: UserNumber;
    name?: string;
  }

  interface Captcha {
    uri: string;
    solution: string;
    retry?: boolean;
  }

  type Register =
    | {
        method: "jwt";
        jwt: string;
      }
    | {
        method: "passkey";
        passkeyIdentity: DiscoverablePasskeyIdentity;
      };

  const { data }: PageProps = $props();

  let showPasskeyModal = $state(false);
  let showNamingPasskey = $state(false);

  // TODO: this should really be pulled from a central store that initializes itself on load
  // TODO: of course we would also need to pull account/profile/role/login info, but one thing
  // TODO: after another
  let lastUsedIdentity: LastUsed | undefined = undefined;
  let continueAsIdentity = $state.raw<LastUsed | undefined>(lastUsedIdentity);
  let captcha = $state<Captcha | undefined>();
  let register = $state<Register | undefined>();
  let passkeyName = $state("");
  let authContext = $state.raw<AuthContext>();
  let dappName = $derived<string>(
    authContext ? authContext?.requestOrigin : "",
  );

  const connection = new Connection(readCanisterId(), readCanisterConfig());

  let onAuthenticate: (
    authenticatedConnection: AuthenticatedConnection,
  ) => void;

  const handleContinueWithPasskey = () => {
    showPasskeyModal = true;
  };

  const handleContinueWithGoogle = async () => {
    const clientId = data.session.config.openid_google?.[0]?.[0]?.client_id;
    if (isNullish(clientId)) {
      return;
    }
    const requestConfig = createGoogleRequestConfig(clientId);
    const jwt = await requestJWT(requestConfig, {
      nonce: data.session.nonce,
      mediation: "required",
    });
    try {
      const { identity, anchorNumber } = await authenticateWithJWT({
        jwt,
        salt: data.session.salt,
        actor: data.session.actor,
      });
      const result = await connection.fromDelegationIdentity(
        anchorNumber,
        identity,
      );
      onAuthenticate(result.connection);
    } catch (error) {
      if (
        isCanisterError<OpenIdDelegationError>(error) &&
        error.type === "NoSuchAnchor"
      ) {
        register = {
          method: "jwt",
          jwt,
        };
        await handleStartRegistration();
        return;
      }
      throw error;
    }
  };

  const handleStartRegistration = async () => {
    try {
      const { next_step } = await data.session.actor
        .identity_registration_start()
        .then(throwCanisterError);
      if ("CheckCaptcha" in next_step) {
        captcha = {
          uri: `data:image/png;base64,${next_step.CheckCaptcha.captcha_png_base64}`,
          solution: "",
        };
        return;
      }
      await handleFinishRegistration();
    } catch (error) {
      // Registration was started already previously, continue as is
      if (
        isCanisterError<IdRegStartError>(error) &&
        error.type === "AlreadyInProgress"
      ) {
        await handleFinishRegistration();
        return;
      }
      throw error;
    }
  };

  const handleValidateCaptcha = async () => {
    if (isNullish(captcha)) {
      return;
    }
    try {
      await data.session.actor
        .check_captcha({ solution: captcha.solution })
        .then(throwCanisterError);
      await handleFinishRegistration();
    } catch (error) {
      if (
        isCanisterError<CheckCaptchaError>(error) &&
        error.type === "WrongSolution"
      ) {
        captcha = {
          uri: `data:image/png;base64,${error.value(error.type).new_captcha_png_base64}`,
          solution: "",
          retry: true,
        };
        return;
      }
      throw error;
    }
  };

  const handleFinishRegistration = async () => {
    switch (register?.method) {
      case "passkey":
        await handleFinishRegistrationWithPasskey();
        break;
      case "jwt":
        await handleFinishRegistrationWithJWT();
        break;
    }
  };

  const handleFinishRegistrationWithJWT = async () => {
    if (isNullish(register) || register.method !== "jwt") {
      return;
    }
    try {
      await data.session.actor
        .openid_identity_registration_finish({
          jwt: register.jwt,
          salt: data.session.salt,
        })
        .then(throwCanisterError);
      const { identity, anchorNumber } = await authenticateWithJWT({
        jwt: register.jwt,
        salt: data.session.salt,
        actor: data.session.actor,
      });
      const result = await connection.fromDelegationIdentity(
        anchorNumber,
        identity,
      );
      onAuthenticate(result.connection);
    } catch (error) {
      // Show CAPTCHA if it was skipped but is required
      if (
        isCanisterError<IdRegFinishError>(error) &&
        error.type === "UnexpectedCall"
      ) {
        const nextStep = error.value(error.type).next_step;
        if ("CheckCaptcha" in nextStep) {
          captcha = {
            uri: `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`,
            solution: "",
          };
          return;
        }
      }
      throw error;
    }
  };

  const handleFinishRegistrationWithPasskey = async () => {
    if (isNullish(register) || register.method !== "passkey") {
      return;
    }
    const uaParser = loadUAParser();
    const alias = await inferPasskeyAlias({
      authenticatorType: register.passkeyIdentity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: register.passkeyIdentity.getAaguid(),
    });
    const name = register.passkeyIdentity.getName();
    const authnMethod = passkeyAuthnMethodData({
      alias,
      pubKey: register.passkeyIdentity.getPublicKey().toDer(),
      credentialId: register.passkeyIdentity.getCredentialId()!,
      authenticatorAttachment:
        register.passkeyIdentity.getAuthenticatorAttachment(),
      origin: window.location.origin,
    });
    try {
      const { identity_number } = await data.session.actor
        .identity_registration_finish({
          name: nonNullish(name) ? [name] : [],
          authn_method: authnMethod,
        })
        .then(throwCanisterError);
      const result = await connection.fromIdentity(
        () => identity_number,
        data.session.identity,
      );
      onAuthenticate(result.connection);
    } catch (error) {
      // Show CAPTCHA if it was skipped but is required
      if (
        isCanisterError<IdRegFinishError>(error) &&
        error.type === "UnexpectedCall"
      ) {
        const nextStep = error.value(error.type).next_step;
        if ("CheckCaptcha" in nextStep) {
          captcha = {
            uri: `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`,
            solution: "",
          };
          return;
        }
      }
      throw error;
    }
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

  const handleCreatePasskey = () => {
    showNamingPasskey = true;
  };

  const handleCancelCreatePasskey = () => {
    showNamingPasskey = false;
    passkeyName = "";
  };

  const handleContinueWithLastUsedIdentity = () => {
    //TODO
    console.log("continuing with last used");
  };

  const handleContinueWithOtherIdentity = () => {
    lastUsedIdentity = undefined;
  };

  const handleStartRegistrationWithPasskey = async () => {
    const passkeyIdentity = await DiscoverablePasskeyIdentity.create({
      publicKey: {
        ...creationOptions([], undefined, undefined),
        user: {
          id: window.crypto.getRandomValues(new Uint8Array(16)),
          name: passkeyName,
          displayName: passkeyName,
        },
      },
    });
    register = {
      method: "passkey",
      passkeyIdentity,
    };
    await handleStartRegistration();
  };

  const handleHidePasskeyModal = () => {
    showPasskeyModal = false;
    showNamingPasskey = false;
    passkeyName = "";
    captcha = undefined;
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
  <div class="margin mb-8 flex flex-col gap-1">
    <h1 class="h1 font-bold">Sign in</h1>
    <p class="p font-medium">
      to continue with <span class="font-bold">{dappName.slice(7, 18)}</span>
    </p>
  </div>
  {#if nonNullish(continueAsIdentity)}
    {@render continueAs(continueAsIdentity)}
  {:else}
    {@render pickAuthenticationMethod()}
  {/if}
{/snippet}

{#snippet continueAs(lastUsedIdentity: LastUsed)}
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
{/snippet}

{#snippet pickAuthenticationMethod()}
  <Button onclick={handleContinueWithPasskey} class="w-full" variant="primary"
    >Continue with Passkey</Button
  >
  <Button onclick={handleContinueWithGoogle} class="w-full" variant="secondary"
    >Continue with Google</Button
  >
  <Button class="w-full" variant="text-only">Cancel</Button>
  {#if showPasskeyModal || nonNullish(captcha)}
    <BottomCardOrModal
      title={nonNullish(captcha)
        ? "Prove you're not a robot"
        : "Continue with Passkey"}
      onclose={handleHidePasskeyModal}
      class="min-h-96"
    >
      {#if nonNullish(captcha)}
        {@render solveCaptcha()}
      {:else if showNamingPasskey}
        {@render nameYourPasskey()}
      {:else}
        {@render connectOrCreatePasskey()}
      {/if}
    </BottomCardOrModal>
  {/if}
{/snippet}

{#snippet solveCaptcha()}
  {#if nonNullish(captcha)}
    <form class="flex flex-1 flex-col gap-8">
      <div class="flex flex-col gap-4">
        <div class="preset-tonal relative flex items-center justify-center">
          <img
            src={captcha.uri}
            class="h-30 w-55 mix-blend-darken dark:mix-blend-lighten dark:invert"
            alt="CAPTCHA Characters"
          />
          <div
            class="dark:bg-ii-background-primary-light bg-ii-background-primary-dark absolute h-30 w-55 mix-blend-lighten dark:mix-blend-darken"
          ></div>
        </div>
        {#if captcha.retry}
          <p class="p">Incorrect solution, try again</p>
        {:else}
          <p class="p">Type the characters you see</p>
        {/if}
        <input
          bind:value={captcha.solution}
          class="input px-4 py-2"
          type="text"
          autofocus
          autocapitalize="none"
          spellcheck="false"
        />
      </div>
      <div class="mt-auto flex flex-col gap-4">
        <Button
          onclick={handleValidateCaptcha}
          class="w-full"
          type="submit"
          disabled={captcha.solution.length === 0}
          variant="primary">Submit</Button
        >
        <Button
          onclick={handleHidePasskeyModal}
          class="w-full"
          variant="secondary">Cancel</Button
        >
      </div>
    </form>
  {/if}
{/snippet}

{#snippet nameYourPasskey()}
  <form class="flex flex-1 flex-col gap-8">
    <div class="flex flex-col gap-4" in:fly={{ duration: 200, x: 10 }}>
      <p>
        Explicabo corrupti temporibus consequuntur quae accusamus eligendi eius,
        ducimus iste iure.
      </p>
      <label class="label">
        <span class="label-text">Name</span>
        <input
          bind:value={passkeyName}
          class="input px-4 py-2"
          type="text"
          autofocus
        />
      </label>
    </div>
    <div class="mt-auto flex flex-col gap-4">
      <Button
        onclick={handleStartRegistrationWithPasskey}
        class="w-full"
        type="submit"
        disabled={passkeyName.length === 0}
        variant="primary">Create Passkey</Button
      >
      <Button
        onclick={handleCancelCreatePasskey}
        class="w-full"
        variant="secondary">Back</Button
      >
    </div>
  </form>
{/snippet}

{#snippet connectOrCreatePasskey()}
  <div class="mb-8 flex flex-col gap-4" in:fly={{ duration: 200, x: -10 }}>
    <p>
      Lorem ipsum dolor sit amet consectetur, adipisicing elit. Doloribus alias
      amet quas, ducimus iste iure et.
    </p>
    <p>
      Boriosam aliquid rerum dolore porro optio, explicabo corrupti temporibus
      consequuntur quae accusamus eligendi eius.
    </p>
  </div>
  <div class="mt-auto flex flex-col gap-4">
    <Button onclick={handleConnectPasskey} class="w-full" variant="primary"
      >Connect my Passkey</Button
    >
    <Button onclick={handleCreatePasskey} class="w-full" variant="secondary"
      >Don't have a passkey? Create one</Button
    >
  </div>
{/snippet}

<!-- an element with data-page 'new-authorize-view' is necessary for the e2e tests to pass -->
<CenterContainer data-page="new-authorize-view">
  <CenterCard>
    {#if nonNullish(authContext)}
      {@render authenticate()}
    {:else}
      <div>Loading...</div>
    {/if}
  </CenterCard>
</CenterContainer>
