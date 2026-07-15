<script lang="ts">
  import { onMount } from "svelte";
  import { BellIcon, XIcon } from "@lucide/svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { listConsentedOrigins, revokeConsent } from "$lib/utils/pushConsent";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  let origins = $state<string[]>([]);
  // True until the initial list read completes.
  let loaded = $state(false);
  // The origin currently being revoked, if any — disables just that row's
  // button rather than the whole list.
  let revoking = $state<string | undefined>(undefined);

  onMount(() => {
    void (async () => {
      try {
        origins = await listConsentedOrigins(
          $authenticatedStore.actor,
          identityNumber,
        );
      } catch {
        toaster.error({
          title: $t`Couldn't load your notification permissions.`,
          duration: 4000,
        });
      } finally {
        loaded = true;
      }
    })();
  });

  const handleRevoke = async (origin: string) => {
    revoking = origin;
    try {
      await revokeConsent($authenticatedStore.actor, identityNumber, origin);
      origins = origins.filter((entry) => entry !== origin);
    } catch {
      toaster.error({
        title: $t`Couldn't remove this permission. Please try again.`,
        duration: 4000,
      });
    } finally {
      revoking = undefined;
    }
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-col gap-4 rounded-xl border p-4 sm:p-5"
>
  <div class="flex flex-row items-start gap-3 sm:gap-4">
    <span
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <BellIcon class="size-5" />
    </span>

    <div class="flex min-h-[2.5rem] min-w-0 flex-1 flex-col justify-center">
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`Push notifications`}
      </h3>
      <p class="text-text-tertiary text-sm">
        <Trans>Apps you've allowed to send you push notifications.</Trans>
      </p>
    </div>
  </div>

  {#if !loaded}
    <div class="flex items-center justify-center py-4">
      <ProgressRing class="text-fg-tertiary size-5" />
    </div>
  {:else if origins.length === 0}
    <p class="text-text-tertiary text-sm">
      {$t`No apps have permission to send you push notifications yet.`}
    </p>
  {:else}
    <ul class="flex flex-col gap-2" aria-labelledby={titleId}>
      {#each origins as origin (origin)}
        <li
          class="border-border-secondary bg-bg-primary flex flex-row items-center gap-2 rounded-lg border px-3 py-2.5 sm:gap-3 sm:px-4"
        >
          <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
            <Ellipsis text={origin} position="middle" />
          </span>
          <Tooltip label={$t`Revoke permission`}>
            <button
              class="btn btn-tertiary btn-sm btn-icon shrink-0"
              onclick={() => void handleRevoke(origin)}
              disabled={revoking !== undefined}
              aria-label={$t`Revoke permission for ${origin}`}
            >
              {#if revoking === origin}
                <ProgressRing class="size-5" />
              {:else}
                <XIcon class="size-5" />
              {/if}
            </button>
          </Tooltip>
        </li>
      {/each}
    </ul>
  {/if}
</section>
