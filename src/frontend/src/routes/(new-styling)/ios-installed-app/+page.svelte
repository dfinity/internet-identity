<script lang="ts">
  import { onMount } from "svelte";
  import CheckIcon from "@lucide/svelte/icons/check";
  import CircleAlertIcon from "@lucide/svelte/icons/circle-alert";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import type { ClaimSameDeviceLinkError } from "$lib/generated/internet_identity_types";
  import { isCanisterError, type CanisterError } from "$lib/utils/utils";
  import {
    claimSameDeviceLink,
    linkKeyOf,
    type LinkOutcome,
  } from "./claimLink";

  /** What the page shows: the claim in flight, how it came out, or why it failed. */
  type Screen =
    | { kind: "working" }
    | { kind: LinkOutcome }
    | { kind: "failed"; reason: string };

  let screen = $state<Screen>({ kind: "working" });

  /** A refusal's variant, with the reason the canister gave where it gave one. */
  const refusalReason = (
    error: CanisterError<ClaimSameDeviceLinkError>,
  ): string =>
    error.type === "InternalCanisterError"
      ? `${error.type}: ${error.value("InternalCanisterError")}`
      : String(error.type);
  let linkKey: Uint8Array | undefined;

  const claim = async () => {
    if (linkKey === undefined) {
      screen = { kind: "not-linked" };
      return;
    }
    try {
      screen = { kind: await claimSameDeviceLink(linkKey) };
    } catch (error) {
      console.error(error);
      screen = {
        kind: "failed",
        reason: isCanisterError<ClaimSameDeviceLinkError>(error)
          ? refusalReason(error)
          : error instanceof Error
            ? error.message
            : String(error),
      };
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

<div class="flex min-h-[100dvh] flex-col items-center justify-center">
  <div
    class="grid w-full flex-1 items-center max-sm:items-stretch sm:w-100 sm:max-w-100"
  >
    <AuthPanel>
      {#if screen.kind === "working"}
        <div
          class="flex min-w-0 flex-1 flex-col items-stretch"
          aria-busy="true"
          aria-label={$t`Setting up notifications`}
        >
          <div class="skeleton mb-4 size-12 rounded-full"></div>
          <div class="skeleton mb-3 h-8 w-3/4 rounded"></div>
          <div class="skeleton h-6 w-full rounded"></div>
        </div>
      {:else if screen.kind === "failed"}
        <div class="flex min-w-0 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <CircleAlertIcon
              class="text-fg-error-primary size-6"
              aria-hidden="true"
            />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Something went wrong`}
          </h1>
          <p
            class="bg-bg-secondary border-border-secondary text-text-tertiary rounded-lg border px-3.5 py-3 font-mono text-xs/4 break-words"
          >
            {screen.reason}
          </p>
        </div>
      {:else}
        <div class="flex min-w-0 flex-1 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <CheckIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {#if screen.kind === "linked"}
              {$t`Done`}
            {:else}
              {$t`Nothing to change`}
            {/if}
          </h1>
          <p class="text-text-tertiary text-base">
            {#if screen.kind === "linked"}
              <Trans
                >From now on, notifications arrive directly in the app.</Trans
              >
            {:else}
              <Trans>
                This iPhone already gets notifications only in the app.
              </Trans>
            {/if}
          </p>
          <p
            class="text-text-tertiary mt-auto flex h-20 items-center justify-center text-sm"
          >
            <Trans>You can close this page.</Trans>
          </p>
        </div>
      {/if}
    </AuthPanel>
  </div>
</div>
