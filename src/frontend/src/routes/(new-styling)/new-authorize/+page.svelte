<script lang="ts">
  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import CenterCard from "$lib/components/UI/CenterCard.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
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
  import { type State } from "./state";
  import ConnectOrCreatePasskey from "./components/ConnectOrCreatePasskey.svelte";
  import CreatePasskey from "./components/CreatePasskey.svelte";
  import SolveCaptcha from "./components/SolveCaptcha.svelte";
  import ContinueAs from "./components/ContinueAs.svelte";
  import Dialog from "$lib/components/UI/Dialog.svelte";
  import Button from "$lib/components/UI/Button.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { handleError } from "./error";

  const { data }: PageProps = $props();

  let currentState = $state<State>({ state: "loading" });
  let authContext = $state.raw<AuthContext>();
  let dappName = $derived<string>(
    authContext ? authContext?.requestOrigin : "",
  );

  let onAuthenticate: (
    authenticatedConnection: AuthenticatedConnection,
    credentialId: ArrayBuffer | undefined,
  ) => void;
  const connection = new Connection(readCanisterId(), readCanisterConfig());

  const pickAuthenticationMethod = () => {
    currentState = { state: "pickAuthenticationMethod" };
  };

  const connectOrCreatePasskey = async () => {
    currentState = {
      state: "connectOrCreatePasskey",
      connect: authenticateWithDiscoverablePasskey,
      create: createPasskey,
    };
  };

  const authenticateWithDiscoverablePasskey = async () => {
    currentState = { state: "loading" };
    try {
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
      onAuthenticate(result.connection, passkeyIdentity.getCredentialId());
    } catch (error) {
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const authenticateWithPasskey = async ({
    anchorNumber,
    credentialId,
  }: {
    anchorNumber: UserNumber;
    credentialId: ArrayBuffer | undefined;
  }) => {
    currentState = { state: "loading" };
    if (!credentialId) {
      console.error("Credential ID is required for passkey authentication");
      pickAuthenticationMethod();
      return;
    }

    try {
      const passkeyIdentity = new DiscoverablePasskeyIdentity({
        credentialRequestOptions: {
          publicKey: {
            allowCredentials: [{ type: "public-key", id: credentialId }],
          },
        },
        getPublicKey: async (result) => {
          const lookupResult = await connection.lookupDeviceKey(
            new Uint8Array(result.rawId),
          );
          if (isNullish(lookupResult)) {
            throw new Error("Account not migrated yet");
          }
          return CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey));
        },
      });

      const delegationIdentity =
        await connection.requestFEDelegation(passkeyIdentity);

      const actor = await connection.createActor(delegationIdentity);

      const authenticatedConnection = new AuthenticatedConnection(
        connection.canisterId,
        connection.canisterConfig,
        passkeyIdentity,
        delegationIdentity,
        anchorNumber,
        actor,
      );

      onAuthenticate(authenticatedConnection, credentialId);
    } catch (err) {
      console.error("Authentication error:", err);
      handleError(err);
      pickAuthenticationMethod();
    }
  };

  const createPasskey = async () => {
    currentState = {
      state: "createPasskey",
      create: async (name: string) => {
        currentState = { state: "loading" };
        try {
          const passkeyIdentity = await DiscoverablePasskeyIdentity.create({
            publicKey: {
              ...creationOptions([], undefined, undefined),
              user: {
                id: window.crypto.getRandomValues(new Uint8Array(16)),
                name,
                displayName: name,
              },
            },
          });
          await startRegistration();
          await registerWithPasskey(passkeyIdentity);
        } catch (error) {
          handleError(error);
          pickAuthenticationMethod();
        }
      },
      cancel: connectOrCreatePasskey,
    };
  };

  const registerWithPasskey = async (
    passkeyIdentity: DiscoverablePasskeyIdentity,
    attempts = 0,
  ) => {
    const uaParser = loadUAParser();
    const alias = await inferPasskeyAlias({
      authenticatorType: passkeyIdentity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: passkeyIdentity.getAaguid(),
    });
    const authnMethod = passkeyAuthnMethodData({
      alias,
      pubKey: passkeyIdentity.getPublicKey().toDer(),
      credentialId: passkeyIdentity.getCredentialId()!,
      authenticatorAttachment: passkeyIdentity.getAuthenticatorAttachment(),
      origin: window.location.origin,
    });
    const name = passkeyIdentity.getName();
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
      onAuthenticate(result.connection, passkeyIdentity.getCredentialId());
    } catch (error) {
      if (isCanisterError<IdRegFinishError>(error)) {
        switch (error.type) {
          case "UnexpectedCall":
            const nextStep = error.value(error.type).next_step;
            if ("CheckCaptcha" in nextStep) {
              if (attempts < 3) {
                await solveCaptcha(
                  `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`,
                );
                return registerWithPasskey(passkeyIdentity, attempts + 1);
              }
            }
            break;
          case "NoRegistrationFlow":
            if (attempts < 3) {
              // Apparently the flow has been cleaned up, try again.
              await startRegistration();
              return await registerWithPasskey(passkeyIdentity, attempts + 1);
            }
            break;
        }
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const authenticateWithGoogle = async () => {
    const clientId = data.session.config.openid_google?.[0]?.[0]?.client_id;
    if (isNullish(clientId)) {
      return;
    }
    currentState = { state: "loading" };
    const requestConfig = createGoogleRequestConfig(clientId);
    let jwt: string | undefined;
    try {
      jwt = await requestJWT(requestConfig, {
        nonce: data.session.nonce,
        mediation: "required",
      });
      const { identity, anchorNumber } = await authenticateWithJWT({
        jwt,
        salt: data.session.salt,
        actor: data.session.actor,
      });
      const result = await connection.fromDelegationIdentity(
        anchorNumber,
        identity,
      );
      onAuthenticate(result.connection, undefined);
    } catch (error) {
      if (
        isCanisterError<OpenIdDelegationError>(error) &&
        error.type === "NoSuchAnchor" &&
        nonNullish(jwt)
      ) {
        await startRegistration();
        return registerWithGoogle(jwt);
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const startRegistration = async (): Promise<void> => {
    try {
      const { next_step } = await data.session.actor
        .identity_registration_start()
        .then(throwCanisterError);
      if ("CheckCaptcha" in next_step) {
        await solveCaptcha(
          `data:image/png;base64,${next_step.CheckCaptcha.captcha_png_base64}`,
        );
      }
    } catch (error) {
      if (
        isCanisterError<IdRegStartError>(error) &&
        error.type === "AlreadyInProgress"
      ) {
        // Ignore since it means we can continue with an existing registration
        return;
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const solveCaptcha = async (captcha: string, attempt = 0): Promise<void> =>
    new Promise((resolve) => {
      currentState = {
        state: "solveCaptcha",
        image: captcha,
        attempt,
        solve: async (solution) => {
          const nextCaptcha = await validateCaptcha(solution);
          if (nonNullish(nextCaptcha)) {
            await solveCaptcha(nextCaptcha, attempt + 1);
          }
          resolve();
        },
        cancel: pickAuthenticationMethod,
      };
    });

  const validateCaptcha = async (
    solution: string,
  ): Promise<string | undefined> => {
    try {
      await data.session.actor
        .check_captcha({ solution })
        .then(throwCanisterError);
    } catch (error) {
      if (
        isCanisterError<CheckCaptchaError>(error) &&
        error.type === "WrongSolution"
      ) {
        return `data:image/png;base64,${error.value(error.type).new_captcha_png_base64}`;
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const registerWithGoogle = async (jwt: string) => {
    try {
      await data.session.actor
        .openid_identity_registration_finish({
          jwt,
          salt: data.session.salt,
        })
        .then(throwCanisterError);
      const { identity, anchorNumber } = await authenticateWithJWT({
        jwt,
        salt: data.session.salt,
        actor: data.session.actor,
      });
      const result = await connection.fromDelegationIdentity(
        anchorNumber,
        identity,
      );
      onAuthenticate(result.connection, undefined);
    } catch (error) {
      if (
        isCanisterError<IdRegFinishError>(error) &&
        error.type === "UnexpectedCall"
      ) {
        const nextStep = error.value(error.type).next_step;
        if ("CheckCaptcha" in nextStep) {
          await solveCaptcha(
            `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`,
          );
          return registerWithGoogle(jwt);
        }
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  authenticationProtocol({
    authenticate: (context) => {
      authContext = context;
      return new Promise((resolve) => {
        onAuthenticate = async (
          authenticatedConnection,
          credentialId: ArrayBuffer | undefined,
        ) => {
          const derivationOrigin =
            context.authRequest.derivationOrigin ?? context.requestOrigin;
          const [result, anchorInfo] = await Promise.all([
            fetchDelegation({
              connection: authenticatedConnection,
              derivationOrigin,
              publicKey: context.authRequest.sessionPublicKey,
              maxTimeToLive: context.authRequest.maxTimeToLive,
            }),
            authenticatedConnection.getAnchorInfo(),
          ]);
          if ("error" in result) {
            return;
          }
          const [userKey, parsed_signed_delegation] = result;
          lastUsedIdentitiesStore.addLatestUsed({
            identityNumber: authenticatedConnection.userNumber,
            name: anchorInfo.name[0],
            credentialId: credentialId
              ? new Uint8Array(credentialId)
              : undefined,
          });
          resolve({
            kind: "success",
            delegations: [parsed_signed_delegation],
            userPublicKey: new Uint8Array(userKey),
            authnMethod: "passkey",
          });
        };
        currentState = nonNullish(data.lastUsedIdentity)
          ? {
              state: "continueAs",
              number: data.lastUsedIdentity.identityNumber,
              name: data.lastUsedIdentity.name,
              continue: () =>
                authenticateWithPasskey({
                  anchorNumber: data.lastUsedIdentity.identityNumber,
                  credentialId: data.lastUsedIdentity.credentialId,
                }),
              useAnother: pickAuthenticationMethod,
            }
          : { state: "pickAuthenticationMethod" };
      });
    },
    onProgress: () => {},
  });
</script>

<CenterContainer data-page="new-authorize-view">
  <CenterCard>
    {#if currentState.state === "loading"}
      <p>Loading...</p>
    {:else if currentState.state === "solveCaptcha"}
      <Dialog
        title={currentState.state === "solveCaptcha"
          ? "Prove you're not a robot"
          : "Continue with Passkey"}
        class="min-h-96 w-100"
      >
        <SolveCaptcha {...currentState} />
      </Dialog>
    {:else}
      <div class="mb-8 flex flex-col gap-1">
        <h1 class="h1 font-bold">Sign in</h1>
        <p class="p font-medium">
          to continue with <span class="font-bold">{dappName.slice(7, 18)}</span
          >
        </p>
      </div>
      {#if currentState.state === "continueAs"}
        <ContinueAs {...currentState} />
      {:else}
        <div class="flex flex-col items-stretch gap-4">
          <Button onclick={connectOrCreatePasskey} variant="primary"
            >Continue with Passkey</Button
          >
          <Button onclick={authenticateWithGoogle} variant="secondary"
            >Continue with Google</Button
          >
          <Button variant="text-only">Cancel</Button>
        </div>
        {#if currentState.state === "connectOrCreatePasskey" || currentState.state === "createPasskey"}
          <Dialog
            title={"Continue with Passkey"}
            onClose={pickAuthenticationMethod}
            class="min-h-96 w-100"
          >
            {#if currentState.state === "connectOrCreatePasskey"}
              <ConnectOrCreatePasskey {...currentState} />
            {:else if currentState.state === "createPasskey"}
              <CreatePasskey {...currentState} />
            {/if}
          </Dialog>
        {/if}
      {/if}
    {/if}
  </CenterCard>
</CenterContainer>
