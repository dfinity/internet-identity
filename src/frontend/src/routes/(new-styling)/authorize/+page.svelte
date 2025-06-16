<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import type {
    CheckCaptchaError,
    IdRegFinishError,
    IdRegStartError,
    OpenIdDelegationError
  } from "$lib/generated/internet_identity_types";
  import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
  import { inferPasskeyAlias, loadUAParser } from "$lib/flows/register";
  import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
  import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
  import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { handleError } from "$lib/components/utils/error";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import PickAuthenticationMethod from "$lib/components/views/PickAuthenticationMethod.svelte";
  import {
    authenticateWithJWT,
    authenticateWithPasskey,
    authenticateWithSession
  } from "$lib/utils/authentication";
  import { canisterConfig, canisterId } from "$lib/globals";
  import { sessionStore } from "$lib/stores/session.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import {
    authenticatedStore,
    authenticationStore
  } from "$lib/stores/authentication.store";
  import { goto } from "$app/navigation";
  import {
    authorizationStore,
    authorizationContextStore
  } from "$lib/stores/authorization.store";
  import { toaster } from "$lib/components/utils/toaster";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import SolveCaptcha from "$lib/components/views/SolveCaptcha.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/views/CreatePasskey.svelte";
  import { onMount } from "svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { getDapps } from "$lib/flows/dappsExplorer/dapps";

  let dialog = $state<"setupOrUseExistingPasskey" | "setupNewPasskey">();
  let captcha = $state<{
    image: string;
    attempt: number;
    solve: (solution: string) => void;
  }>();
  let systemOverlay = $state(false);
  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin)
    )
  );

  const setupOrUseExistingPasskey = async () => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.ContinueWithPasskeyScreen
    );
    dialog = "setupOrUseExistingPasskey";
  };

  const continueWithExistingPasskey = async () => {
    try {
      authenticationV2Funnel.trigger(AuthenticationV2Events.UseExistingPasskey);
      const { identity, identityNumber, credentialId } =
        await authenticateWithPasskey({
          canisterId,
          session: $sessionStore
        });
      authenticationStore.set({ identity, identityNumber });
      const info =
        await $authenticatedStore.actor.get_anchor_info(identityNumber);
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: { passkey: { credentialId } }
      });
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      await goto("/authorize/account");
    } catch (error) {
      handleError(error);
      dialog = undefined;
    }
  };

  const setupNewPasskey = () => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.EnterNameScreen);
    dialog = "setupNewPasskey";
  };

  const createPasskey = async (name: string) => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.StartWebauthnCreation
    );
    try {
      const passkeyIdentity = await DiscoverablePasskeyIdentity.createNew(name);
      await startRegistration();
      await registerWithPasskey(passkeyIdentity);
    } catch (error) {
      handleError(error);
      dialog = undefined;
    }
  };

  const registerWithPasskey = async (
    passkeyIdentity: DiscoverablePasskeyIdentity,
    attempts = 0
  ) => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.RegisterWithPasskey);
    const uaParser = loadUAParser();
    const alias = await inferPasskeyAlias({
      authenticatorType: passkeyIdentity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: passkeyIdentity.getAaguid()
    });
    const authnMethod = passkeyAuthnMethodData({
      alias,
      pubKey: passkeyIdentity.getPublicKey().toDer(),
      credentialId: passkeyIdentity.getCredentialId()!,
      authenticatorAttachment: passkeyIdentity.getAuthenticatorAttachment(),
      origin: window.location.origin
    });
    const name = passkeyIdentity.getName();
    try {
      const { identity_number: identityNumber } = await $sessionStore.actor
        .identity_registration_finish({
          name: nonNullish(name) ? [name] : [],
          authn_method: authnMethod
        })
        .then(throwCanisterError);
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulPasskeyRegistration
      );
      const credentialId = new Uint8Array(passkeyIdentity.getCredentialId()!);
      const identity = await authenticateWithSession({
        session: $sessionStore
      });
      authenticationStore.set({ identity, identityNumber });
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: passkeyIdentity.getName(),
        authMethod: { passkey: { credentialId } }
      });
      lastUsedIdentitiesStore.addLastUsedAccount({
        origin: $authorizationContextStore.effectiveOrigin,
        identityNumber,
        accountNumber: undefined
      });
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      captcha = undefined;
      toaster.success({
        title: "You're all set. Your identity has been created.",
        duration: 4000,
        closable: false
      });
      await authorizationStore.authorize(undefined, 4000);
    } catch (error) {
      if (isCanisterError<IdRegFinishError>(error)) {
        switch (error.type) {
          case "UnexpectedCall":
            const nextStep = error.value(error.type).next_step;
            if ("CheckCaptcha" in nextStep) {
              if (attempts < 3) {
                await solveCaptcha(
                  `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`
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
      dialog = undefined;
    }
  };

  const continueWithGoogle = async () => {
    let jwt: string | undefined;
    try {
      authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueWithGoogle);
      const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id!;
      const requestConfig = createGoogleRequestConfig(clientId);
      systemOverlay = true;
      jwt = await requestJWT(requestConfig, {
        nonce: $sessionStore.nonce,
        mediation: "required"
      });
      systemOverlay = false;
      const { identity, identityNumber, iss, sub } = await authenticateWithJWT({
        canisterId,
        session: $sessionStore,
        jwt
      });
      // If the previous call succeeds, it means the Google user already exists in II.
      // Therefore, they are logging in.
      // If the call fails, it means the Google user does not exist in II.
      // In that case, we register them.
      authenticationV2Funnel.trigger(AuthenticationV2Events.LoginWithGoogle);
      authenticationStore.set({ identity, identityNumber });
      const info =
        await $authenticatedStore.actor.get_anchor_info(identityNumber);
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: { openid: { iss, sub } }
      });
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      await goto("/authorize/account");
    } catch (error) {
      systemOverlay = false;
      if (
        isCanisterError<OpenIdDelegationError>(error) &&
        error.type === "NoSuchAnchor" &&
        nonNullish(jwt)
      ) {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.RegisterWithGoogle
        );
        await startRegistration();
        return registerWithGoogle(jwt);
      }
      handleError(error);
      dialog = undefined;
    }
  };

  const startRegistration = async (): Promise<void> => {
    try {
      const { next_step } = await $sessionStore.actor
        .identity_registration_start()
        .then(throwCanisterError);
      if ("CheckCaptcha" in next_step) {
        await solveCaptcha(
          `data:image/png;base64,${next_step.CheckCaptcha.captcha_png_base64}`
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
      dialog = undefined;
    }
  };

  const solveCaptcha = async (image: string, attempt = 0): Promise<void> =>
    new Promise((resolve) => {
      captcha = {
        image,
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
              const nextImage = `data:image/png;base64,${error.value(error.type).new_captcha_png_base64}`;
              await solveCaptcha(nextImage, attempt + 1);
              resolve();
              return;
            }
            handleError(error);
            dialog = undefined;
          }
        }
      };
    });

  const registerWithGoogle = async (jwt: string) => {
    try {
      await $sessionStore.actor
        .openid_identity_registration_finish({
          jwt,
          salt: $sessionStore.salt
        })
        .then(throwCanisterError);
      const { identity, identityNumber, iss, sub } = await authenticateWithJWT({
        canisterId,
        session: $sessionStore,
        jwt
      });
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulGoogleRegistration
      );
      authenticationStore.set({ identity, identityNumber });
      const info =
        await $authenticatedStore.actor.get_anchor_info(identityNumber);
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: { openid: { iss, sub } }
      });
      lastUsedIdentitiesStore.addLastUsedAccount({
        origin: $authorizationContextStore.effectiveOrigin,
        identityNumber,
        accountNumber: undefined
      });
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      captcha = undefined;
      toaster.success({
        title: "You're all set. Your identity has been created.",
        duration: 4000,
        closable: false
      });
      await authorizationStore.authorize(undefined, 4000);
    } catch (error) {
      if (
        isCanisterError<IdRegFinishError>(error) &&
        error.type === "UnexpectedCall"
      ) {
        const nextStep = error.value(error.type).next_step;
        if ("CheckCaptcha" in nextStep) {
          await solveCaptcha(
            `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`
          );
          return registerWithGoogle(jwt);
        }
      }
      handleError(error);
      dialog = undefined;
    }
  };

  onMount(() => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.SelectMethodScreen);
  });
</script>

{#if nonNullish(captcha)}
  <SolveCaptcha {...captcha} />
{:else}
  <AuthorizeHeader origin={$authorizationContextStore.requestOrigin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    Choose method
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    <span>to continue with</span>
    {#if nonNullish(dapp?.name)}
      <span><b>{dapp.name}</b></span>
    {:else}
      <span>this app</span>
    {/if}
  </p>
  <PickAuthenticationMethod {setupOrUseExistingPasskey} {continueWithGoogle} />
  {#if nonNullish(dialog)}
    <Dialog onClose={() => (dialog = undefined)}>
      {#if dialog === "setupOrUseExistingPasskey"}
        <SetupOrUseExistingPasskey
          setupNew={setupNewPasskey}
          useExisting={continueWithExistingPasskey}
        />
      {:else if dialog === "setupNewPasskey"}
        <CreatePasskey create={createPasskey} />
      {/if}
    </Dialog>
  {/if}
{/if}
{#if systemOverlay}
  <SystemOverlayBackdrop />
{/if}
