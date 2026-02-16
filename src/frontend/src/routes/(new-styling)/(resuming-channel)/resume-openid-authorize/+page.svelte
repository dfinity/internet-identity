<script lang="ts">
  import { onMount } from "svelte";
  import {
    authorizationContextStore,
    authorizationStore,
  } from "$lib/stores/authorization.store";
  import Button from "$lib/components/ui/Button.svelte";
  import { ArrowRightIcon } from "@lucide/svelte";
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

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );

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
        (attribute) => attribute.startsWith(`openid:${issuer}:`),
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
  {#if dapp?.logoSrc !== undefined}
    {@const name = dapp?.name ?? $authorizationContextStore.requestOrigin}
    <img
      src={dapp.logoSrc}
      alt={$t`${name} logo`}
      class={[
        "mb-10 h-16 max-w-50 object-contain",
        dapp.logoDarkSrc !== undefined && "dark:hidden",
      ]}
    />
    {#if dapp.logoDarkSrc !== undefined}
      <img
        src={dapp.logoDarkSrc}
        alt={$t`${name} logo`}
        class="mb-10 hidden h-16 max-w-50 object-contain dark:block"
        aria-hidden="true"
      />
    {/if}
  {/if}
  <p class="text-text-secondary mb-1 text-xl font-semibold">
    {$t`Signing in securely`}
  </p>
  <p class="text-text-tertiary text-sm">{$t`This takes a few seconds.`}</p>
  <div class="bg-bg-quaternary my-6 h-0.5 w-full max-w-74 rounded-full">
    <div class="bg-fg-brand-primary animate-grow h-full rounded-full"></div>
  </div>
  <p class="text-text-secondary text-base">
    {$t`Powered by Internet Identity`}
  </p>
  <Button
    href={window.location.origin}
    target="_blank"
    variant="tertiary"
    class="mt-10"
    size="sm"
  >
    <span>{$t`How it works`}</span>
    <ArrowRightIcon class="size-4" />
  </Button>
</div>

<style>
  @keyframes grow {
    from {
      width: 0;
    }
    to {
      width: 100%;
    }
  }

  .animate-grow {
    animation: grow 6s ease-out forwards;
  }
</style>
