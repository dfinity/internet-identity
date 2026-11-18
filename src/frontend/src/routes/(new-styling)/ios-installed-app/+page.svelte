<script lang="ts">
  import { onMount } from "svelte";
  import CheckIcon from "@lucide/svelte/icons/check";
  import CircleAlertIcon from "@lucide/svelte/icons/circle-alert";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { handleError } from "$lib/components/utils/error";
  import {
    claimSameDeviceLink,
    linkKeyOf,
    type LinkOutcome,
  } from "./claimLink";

  let screen = $state<"working" | "failed" | LinkOutcome>("working");
  let linkKey: Uint8Array | undefined;

  const claim = async () => {
    if (linkKey === undefined) {
      screen = "not-linked";
      return;
    }
    screen = "working";
    try {
      screen = await claimSameDeviceLink(linkKey);
    } catch (error) {
      handleError(error);
      screen = "failed";
    }
  };

  onMount(() => {
    linkKey = linkKeyOf(window.location.hash);
    // Kept only in memory from here: a URL holding the key outlives the claim in history.
    history.replaceState(null, "", window.location.pathname);
    void claim();
  });
</script>

<svelte:head>
  <meta name="robots" content="noindex, nofollow" />
</svelte:head>

<div
  class="grid w-full flex-1 items-center max-sm:items-stretch sm:w-100 sm:max-w-100"
>
  <AuthPanel>
    {#if screen === "working"}
      <div class="flex min-w-0 flex-1 flex-col items-stretch" aria-busy="true">
        <div class="skeleton mb-4 size-12 rounded-full"></div>
        <div class="skeleton mb-3 h-8 w-3/4 rounded"></div>
        <div class="skeleton h-6 w-full rounded"></div>
      </div>
    {:else if screen === "linked"}
      <div class="flex min-w-0 flex-col items-stretch">
        <FeaturedIcon size="lg" class="mb-4 self-start">
          <CheckIcon class="size-6" aria-hidden="true" />
        </FeaturedIcon>
        <h1 class="text-text-primary mb-3 text-2xl font-medium">
          {$t`Notifications will arrive in the app`}
        </h1>
        <p class="text-text-tertiary text-base">
          <Trans>
            This iPhone now gets them only in the app you came from, not here as
            well. You can close this page.
          </Trans>
        </p>
      </div>
    {:else if screen === "not-linked"}
      <div class="flex min-w-0 flex-col items-stretch">
        <FeaturedIcon size="lg" class="mb-4 self-start">
          <CircleAlertIcon class="size-6" aria-hidden="true" />
        </FeaturedIcon>
        <h1 class="text-text-primary mb-3 text-2xl font-medium">
          {$t`This link can't be used here`}
        </h1>
        <p class="text-text-tertiary text-base">
          <Trans>
            Sign in to Internet Identity here with the identity you use in the
            app, then tap the button in the app again.
          </Trans>
        </p>
      </div>
    {:else}
      <div class="flex min-w-0 flex-col items-stretch">
        <button class="btn btn-primary btn-xl" onclick={() => void claim()}>
          {$t`Try again`}
        </button>
      </div>
    {/if}
  </AuthPanel>
</div>
