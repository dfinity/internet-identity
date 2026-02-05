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
  import { DelegationResultSchema } from "$lib/utils/transport/utils";

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) => dapp.hasOrigin($authorizationStore.requestOrigin)),
  );

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
      const authFlowResult = await authFlow.continueWithOpenId(config, jwt);
      if (authFlowResult.type === "signUp") {
        await authFlow.completeOpenIdRegistration(authFlowResult.name!);
      }
      const { delegationChain } = await authorizationStore.authorize(undefined);
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
    {@const name = dapp?.name ?? $authorizationStore.requestOrigin}
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
