<script lang="ts">
  import { analytics } from "$lib/utils/analytics/analytics";
  import { onMount } from "svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import {
    authorizationStatusStore,
    authorizationStore,
  } from "$lib/stores/authorization.store";
  import AuthorizeError from "$lib/components/views/AuthorizeError.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { ArrowRightIcon } from "@lucide/svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { decodeJWT, findConfig } from "$lib/utils/openID";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import { t } from "$lib/stores/locale.store";
  import { channelStore } from "$lib/stores/channelStore";
  import { z } from "zod";
  import {
    DelegationParamsSchema,
    DelegationResultSchema,
  } from "$lib/utils/transport/utils";

  analytics.event("page-redirect-callback");

  const status = $derived($authorizationStatusStore);
  let directAuthorizeOrigin = $state<string>();
  const dapps = getDapps();
  const dapp = $derived(
    dapps.find(
      (dapp) =>
        nonNullish(directAuthorizeOrigin) &&
        dapp.hasOrigin(directAuthorizeOrigin),
    ),
  );

  onMount(async () => {
    // If OpenID flow was opened within same window as II,
    // authorize with default account directly instead.
    const searchParams = new URLSearchParams(window.location.hash.slice(1));
    const redirectState = searchParams.get("state");
    const jwt = searchParams.get("id_token");
    const item = sessionStorage.getItem("ii-direct-authorize-openid");
    if (nonNullish(item)) {
      const { origin, state } = JSON.parse(item);
      if (
        nonNullish(redirectState) &&
        redirectState === state &&
        nonNullish(jwt)
      ) {
        directAuthorizeOrigin = origin;

        const authFlow = new AuthFlow({ trackLastUsed: false });
        const { iss, ...metadata } = decodeJWT(jwt);
        const config = findConfig(
          iss,
          Object.entries(metadata).map(([key, value]) => [
            key,
            { String: value! },
          ]),
        );
        if (isNullish(config)) {
          return;
        }
        const authFlowResult = await authFlow.continueWithOpenId(config, jwt);
        if (authFlowResult.type === "signUp") {
          await authFlow.completeOpenIdRegistration(authFlowResult.name!);
        }
        const channel = await channelStore.establish({ allowedOrigin: origin });
        channel.addEventListener("request", async (request) => {
          if (
            request.id === undefined ||
            request.method !== "icrc34_delegation" ||
            $authorizationStore.status !== "init"
          ) {
            return;
          }
          const paramsResult = DelegationParamsSchema.safeParse(request.params);
          if (!paramsResult.success) {
            await channel.send({
              jsonrpc: "2.0",
              id: request.id,
              error: {
                code: -32602,
                message: z.prettifyError(paramsResult.error),
              },
            });
            return;
          }
          await authorizationStore.handleRequest(
            channel.origin,
            request.id,
            paramsResult.data,
          );
          const { delegationChain } =
            await authorizationStore.authorize(undefined);
          const result = DelegationResultSchema.encode(delegationChain);
          await channel.send({ jsonrpc: "2.0", id: request.id, result });
        });
      }
    }

    // User was returned here after redirect from a OpenID flow callback,
    // these flows are always handled in a popup and the callback url is
    // returned to the opener window through the PostMessage API.
    window.opener.postMessage(window.location.href, window.location.origin);
  });
</script>

{#if nonNullish(directAuthorizeOrigin)}
  <div class="flex min-h-[100dvh] flex-col items-center justify-center px-8">
    {#if nonNullish(dapp?.logoSrc)}
      {@const name = dapp?.name ?? directAuthorizeOrigin}
      <img
        src={dapp?.logoSrc}
        alt={$t`${name} logo`}
        class="mb-10 h-16 max-w-50 object-contain"
      />
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
{/if}

<!-- Renders any error status or late success status dialog when needed -->
<AuthorizeError {status} />

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
