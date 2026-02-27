<script lang="ts">
  import { onMount } from "svelte";
  import {
    authorizationContextStore,
    authorizationStore,
  } from "$lib/stores/authorization.store";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { decodeJWT, findConfig } from "$lib/utils/openID";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import { t } from "$lib/stores/locale.store";
  import { establishedChannelStore } from "$lib/stores/channelStore";
  import {
    AttributesParamsSchema,
    DelegationResultSchema,
    INVALID_PARAMS_ERROR_CODE,
    type JsonRequest,
  } from "$lib/utils/transport/utils";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { retryFor, throwCanisterError } from "$lib/utils/utils";
  import { z } from "zod";
  import { canisterConfig } from "$lib/globals";
  import {
    DirectOpenIdEvents,
    directOpenIdFunnel,
  } from "$lib/utils/analytics/DirectOpenIdFunnel";
  import { draw, fade, scale } from "svelte/transition";
  import { cubicOut } from "svelte/easing";
  import { triggerDropWaveAnimation } from "$lib/utils/animation-dispatcher";
  import Logo from "$lib/components/ui/Logo.svelte";

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );

  let animateTransitions = $state(false);

  const createAttributesListener =
    (issuer: string) => async (request: JsonRequest) => {
      if (request.id === undefined || request.method !== "ii_attributes") {
        return;
      }
      const paramsResult = AttributesParamsSchema.safeParse(request.params);
      if (!paramsResult.success) {
        await $establishedChannelStore.send({
          jsonrpc: "2.0",
          id: request.id,
          error: {
            code: INVALID_PARAMS_ERROR_CODE,
            message: z.prettifyError(paramsResult.error),
          },
        });
        return;
      }
      const implicitConsentAttributeKeys = paramsResult.data.attributes.filter(
        (attribute) =>
          attribute === `openid:${issuer}:name` ||
          attribute === `openid:${issuer}:email`,
      );
      try {
        const { attributes, issued_at_timestamp_ns } =
          await $authenticatedStore.actor
            .prepare_attributes({
              origin: $authorizationContextStore.effectiveOrigin,
              attribute_keys: implicitConsentAttributeKeys,
              account_number: [],
              identity_number: $authenticatedStore.identityNumber,
            })
            .then(throwCanisterError);
        const { certified_attributes, expires_at_timestamp_ns } =
          await retryFor(5, () =>
            $authenticatedStore.actor
              .get_attributes({
                origin: $authorizationContextStore.effectiveOrigin,
                account_number: [],
                identity_number: $authenticatedStore.identityNumber,
                attributes,
                issued_at_timestamp_ns,
              })
              .then(throwCanisterError),
          );
        await $establishedChannelStore.send({
          jsonrpc: "2.0",
          id: request.id,
          result: {
            attributes: Object.fromEntries(
              certified_attributes.map((attribute) => [
                attribute.key,
                {
                  value: z.util.uint8ArrayToBase64(
                    new Uint8Array(attribute.value),
                  ),
                  signature: z.util.uint8ArrayToBase64(
                    new Uint8Array(attribute.signature),
                  ),
                  expiration: expires_at_timestamp_ns.toString(),
                },
              ]),
            ),
          },
        });
      } catch (error) {
        await $establishedChannelStore.send({
          jsonrpc: "2.0",
          id: request.id,
          error: {
            code: 1000,
            message: $t`Encountered an internal error while processing the request.`,
          },
        });
      }
    };

  onMount(async () => {
    const searchParams = new URLSearchParams(window.location.hash.slice(1));
    window.history.replaceState(
      undefined,
      "",
      window.location.origin + "/authorize",
    );
    const redirectState = searchParams.get("state");
    const jwt = searchParams.get("id_token");
    const openIdAuthorizeState = sessionStorage.getItem(
      "ii-openid-authorize-state",
    );
    if (
      openIdAuthorizeState !== null &&
      redirectState === openIdAuthorizeState &&
      jwt !== null
    ) {
      const authFlow = new AuthFlow({ trackLastUsed: false });
      const { iss, ...metadata } = decodeJWT(jwt);
      const config = findConfig(
        iss,
        Object.entries(metadata).map(([key, value]) => [
          key,
          { String: value! },
        ]),
      );
      if (config === undefined) {
        return;
      }
      // Animate transitions and background
      animateTransitions = true;
      triggerDropWaveAnimation();

      // Authenticate and respond to requests
      directOpenIdFunnel.addProperties({
        openid_issuer: config.issuer,
      });
      directOpenIdFunnel.trigger(DirectOpenIdEvents.CallbackFromOpenId);
      const authFlowResult = await authFlow.continueWithOpenId(config, jwt);
      if (authFlowResult.type === "signUp") {
        await authFlow.completeOpenIdRegistration(authFlowResult.name!);
      }
      if (
        dapp?.certifiedAttributes === true ||
        canisterConfig.dummy_auth[0] !== undefined
      ) {
        const listener = createAttributesListener(config.issuer);
        void $establishedChannelStore.addEventListener("request", listener);
      }
      const { delegationChain } = await authorizationStore.authorize(undefined);
      directOpenIdFunnel.trigger(DirectOpenIdEvents.RedirectToApp);
      await $establishedChannelStore.send({
        jsonrpc: "2.0",
        id: $authorizationContextStore.requestId,
        result: DelegationResultSchema.encode(delegationChain),
      });
    }
  });
</script>

<div class="flex min-h-[100dvh] flex-col items-center justify-center px-8">
  {#if animateTransitions}
    <div
      transition:scale={{ duration: 500, easing: cubicOut, start: 0.9 }}
      class="flex flex-col items-center justify-center"
    >
      {#if dapp?.logoSrc !== undefined}
        <div class="relative">
          <svg viewBox="0 0 92 92" width="92" height="92" class="mb-4">
            <path
              d="M 46 1 H 71 A 20 20 0 0 1 91 21 V 71 A 20 20 0 0 1 71 91 H 21 A 20 20 0 0 1 1 71 V 21 A 20 20 0 0 1 21 1 H 46"
              class="stroke-border-tertiary fill-none stroke-2"
            />
            <g
              transition:fade|global={{
                duration: 500,
                easing: cubicOut,
                delay: 500,
              }}
            >
              <path
                transition:draw|global={{
                  duration: 6000,
                  easing: cubicOut,
                  delay: 500,
                }}
                d="M 46 1 H 71 A 20 20 0 0 1 91 21 V 71 A 20 20 0 0 1 71 91 H 21 A 20 20 0 0 1 1 71 V 21 A 20 20 0 0 1 21 1 H 46"
                stroke-linecap="round"
                class="stroke-fg-primary fill-none stroke-2"
              />
            </g>
          </svg>
          <img
            src={dapp.logoSrc}
            alt={$t`${dapp.name} logo`}
            class="absolute inset-1 size-[84px] rounded-[18px] object-cover"
          />
        </div>
      {:else}
        <svg viewBox="0 0 92 92" width="92" height="92" class="mb-4">
          <circle
            cx="46"
            cy="46"
            r="45"
            stroke-width="2"
            class="stroke-border-tertiary fill-none stroke-2"
          />
          <g
            transition:fade|global={{
              duration: 500,
              easing: cubicOut,
              delay: 500,
            }}
          >
            <circle
              transition:draw|global={{
                duration: 6000,
                easing: cubicOut,
                delay: 500,
              }}
              cx="46"
              cy="46"
              r="45"
              stroke-linecap="round"
              class="stroke-fg-primary origin-center -rotate-90 fill-none stroke-2"
            />
          </g>
        </svg>
      {/if}
      <p class="text-text-primary mb-2 text-2xl font-medium">
        {$t`Signing in securely`}
      </p>
      <a
        href={window.location.origin}
        target="_blank"
        class="text-text-secondary flex flex-row items-center gap-2 text-base"
      >
        <span>{$t`Powered by`}</span>
        <Logo class="text-text-secondary h-2" />
        <span>{window.location.hostname}</span>
      </a>
    </div>
  {/if}
</div>
