<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import { creationOptions } from "$lib/utils/iiConnection";
  import type {
    CheckCaptchaError,
    IdRegFinishError,
    IdRegStartError,
    OpenIdDelegationError,
  } from "$lib/generated/internet_identity_types";
  import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
  import { inferPasskeyAlias, loadUAParser } from "$lib/flows/register";
  import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
  import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
  import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
  import { type State } from "./state";
  import ConnectOrCreatePasskey from "./components/ConnectOrCreatePasskey.svelte";
  import CreatePasskey from "./components/CreatePasskey.svelte";
  import SolveCaptcha from "./components/SolveCaptcha.svelte";
  import Dialog from "$lib/components/UI/Dialog.svelte";
  import { handleError } from "./error";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import PickAuthenticationMethod from "./components/PickAuthenticationMethod.svelte";
  import {
    authenticateWithJWT,
    authenticateWithPasskey,
    authenticateWithSession,
  } from "$lib/utils/authentication";
  import { canisterConfig, canisterId } from "$lib/globals";
  import { sessionStore } from "$lib/stores/session.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { goto } from "$app/navigation";
  import { authorizationStore } from "$lib/stores/authorization.store";

  let currentState = $state<State>({ state: "pickAuthenticationMethod" });

  const pickAuthenticationMethod = () => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.SelectMethodScreen);
    currentState = { state: "pickAuthenticationMethod" };
  };

  const connectOrCreatePasskey = async () => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.ContinueWithPasskeyScreen,
    );
    currentState = {
      state: "connectOrCreatePasskey",
      connect: continueWithExistingPasskey,
      create: createPasskey,
    };
  };

  const continueWithExistingPasskey = async () => {
    try {
      authenticationV2Funnel.trigger(AuthenticationV2Events.UseExistingPasskey);
      const { identity, identityNumber, credentialId } =
        await authenticateWithPasskey({
          canisterId,
          session: $sessionStore,
        });
      authenticationStore.set({ identity, identityNumber });
      const info =
        await $authenticationStore.actor.get_anchor_info(identityNumber);
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: { passkey: { credentialId } },
      });
      await goto("/authorize/account");
    } catch (error) {
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const createPasskey = () => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.EnterNameScreen);
    currentState = {
      state: "createPasskey",
      create: async (name: string) => {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.StartWebauthnCreation,
        );
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
    authenticationV2Funnel.trigger(AuthenticationV2Events.RegisterWithPasskey);
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
      const { identity_number: identityNumber } = await $sessionStore.actor
        .identity_registration_finish({
          name: nonNullish(name) ? [name] : [],
          authn_method: authnMethod,
        })
        .then(throwCanisterError);
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulPasskeyRegistration,
      );
      const credentialId = new Uint8Array(passkeyIdentity.getCredentialId()!);
      const identity = await authenticateWithSession({
        session: $sessionStore,
      });
      authenticationStore.set({ identity, identityNumber });
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: passkeyIdentity.getName(),
        authMethod: { passkey: { credentialId } },
      });
      lastUsedIdentitiesStore.addLastUsedAccount({
        origin:
          $authorizationStore.authRequest.derivationOrigin ??
          $authorizationStore.requestOrigin,
        identityNumber,
        accountNumber: undefined,
      });
      await authorizationStore.authorize(undefined);
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

  const continueWithGoogle = async () => {
    let jwt: string | undefined;
    try {
      authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueWithGoogle);
      const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id!;
      const requestConfig = createGoogleRequestConfig(clientId);
      jwt = await requestJWT(requestConfig, {
        nonce: $sessionStore.nonce,
        mediation: "required",
      });
      const { identity, identityNumber, iss, sub } = await authenticateWithJWT({
        canisterId,
        session: $sessionStore,
        jwt,
      });
      // If the previous call succeeds, it means the Google user already exists in II.
      // Therefore, they are logging in.
      // If the call fails, it means the Google user does not exist in II.
      // In that case, we register them.
      authenticationV2Funnel.trigger(AuthenticationV2Events.LoginWithGoogle);
      authenticationStore.set({ identity, identityNumber });
      const info =
        await $authenticationStore.actor.get_anchor_info(identityNumber);
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: { openid: { iss, sub } },
      });
      await goto("/authorize/account");
    } catch (error) {
      if (
        isCanisterError<OpenIdDelegationError>(error) &&
        error.type === "NoSuchAnchor" &&
        nonNullish(jwt)
      ) {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.RegisterWithGoogle,
        );
        await startRegistration();
        return registerWithGoogle(jwt);
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const startRegistration = async (): Promise<void> => {
    try {
      const { next_step } = await $sessionStore.actor
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
          try {
            await $sessionStore.actor
              .check_captcha({ solution })
              .then(throwCanisterError);
            resolve();
          } catch (error) {
            if (
              isCanisterError<CheckCaptchaError>(error) &&
              error.type === "WrongSolution"
            ) {
              const nextCaptcha = `data:image/png;base64,${error.value(error.type).new_captcha_png_base64}`;
              await solveCaptcha(nextCaptcha, attempt + 1);
              resolve();
              return;
            }
            handleError(error);
            pickAuthenticationMethod();
          }
        },
        cancel: pickAuthenticationMethod,
      };
    });

  const registerWithGoogle = async (jwt: string) => {
    try {
      await $sessionStore.actor
        .openid_identity_registration_finish({
          jwt,
          salt: $sessionStore.salt,
        })
        .then(throwCanisterError);
      const { identity, identityNumber, iss, sub } = await authenticateWithJWT({
        canisterId,
        session: $sessionStore,
        jwt,
      });
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulGoogleRegistration,
      );
      authenticationStore.set({ identity, identityNumber });
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        authMethod: { openid: { iss, sub } },
      });
      lastUsedIdentitiesStore.addLastUsedAccount({
        origin:
          $authorizationStore.authRequest.derivationOrigin ??
          $authorizationStore.requestOrigin,
        identityNumber,
        accountNumber: undefined,
      });
      await authorizationStore.authorize(undefined);
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
</script>

{#if currentState.state === "solveCaptcha"}
  <Dialog
    title={currentState.state === "solveCaptcha"
      ? "Prove you're not a robot"
      : "Continue with Passkey"}
    class="min-h-96 w-100"
  >
    <SolveCaptcha {...currentState} />
  </Dialog>
{:else}
  <PickAuthenticationMethod {connectOrCreatePasskey} {continueWithGoogle} />
  {#if currentState.state !== "pickAuthenticationMethod"}
    <Dialog
      title={"Continue with Passkey"}
      onClose={pickAuthenticationMethod}
      class="min-h-100 w-100"
    >
      {#if currentState.state === "connectOrCreatePasskey"}
        <ConnectOrCreatePasskey {...currentState} />
      {:else if currentState.state === "createPasskey"}
        <CreatePasskey {...currentState} />
      {/if}
    </Dialog>
  {/if}
{/if}
