<script lang="ts">
  import { onMount } from "svelte";
  import { BellIcon, BellOffIcon } from "@lucide/svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { throwTextCanisterError } from "$lib/utils/utils";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  const dapps = getDapps();
  const appName = (origin: string): string =>
    dapps.find((dapp) => dapp.hasOrigin(origin))?.name ?? origin;

  // `undefined` while the first read is in flight.
  let origins = $state<string[]>();
  let revoking = $state<string>();

  onMount(() => {
    void (async () => {
      try {
        origins =
          await $authenticatedStore.actor.notification_consented_origins(
            identityNumber,
          );
      } catch {
        origins = [];
      }
    })();
  });

  const revoke = async (origin: string) => {
    revoking = origin;
    const previous = origins ?? [];
    origins = previous.filter((consented) => consented !== origin);
    try {
      await $authenticatedStore.actor
        .notification_revoke_consent(identityNumber, origin)
        .then(throwTextCanisterError);
    } catch {
      // Only a failed revoke rolls back — the row must not read "off" while the
      // app can still notify.
      origins = previous;
      toaster.error({
        title: $t`Couldn't turn off notifications. Please try again.`,
        duration: 4000,
      });
      revoking = undefined;
      return;
    }
    // Consent is gone; drop the service worker's credential for this app.
    // Best-effort — a leftover is inert without consent — and lazily imported
    // so its IndexedDB open stays off this page.
    void import("$lib/utils/notifications/pullCredential")
      .then(({ purgeNotificationCredential }) =>
        purgeNotificationCredential(origin),
      )
      .catch(() => {});
    revoking = undefined;
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-col rounded-xl border p-4 sm:p-5"
>
  <div class="flex flex-row items-start gap-3 sm:gap-4">
    <span
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <BellIcon class="size-5" />
    </span>

    <div class="flex min-w-0 flex-1 flex-col gap-1">
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`Notifications`}
      </h3>
      <p class="text-text-tertiary text-sm">
        {$t`Choose which apps can send notifications to your devices.`}
      </p>
    </div>
  </div>

  {#if origins !== undefined && origins.length > 0}
    <ul
      class="border-border-tertiary mt-5 flex flex-col gap-3 border-t pt-4"
      aria-labelledby={titleId}
    >
      {#each origins as origin (origin)}
        <li
          class="border-border-tertiary bg-bg-primary flex flex-row items-center gap-3 rounded-lg border px-3 py-3 sm:px-4"
        >
          <div class="flex min-w-0 flex-1 flex-col">
            <span class="text-text-primary truncate text-sm font-semibold">
              {appName(origin)}
            </span>
            <span class="text-text-tertiary truncate text-xs" title={origin}>
              {origin}
            </span>
          </div>
          <button
            class="btn btn-secondary btn-sm shrink-0 gap-2"
            onclick={() => revoke(origin)}
            disabled={revoking === origin}
          >
            <BellOffIcon class="size-4" />
            {$t`Turn off`}
          </button>
        </li>
      {/each}
    </ul>
  {:else if origins !== undefined}
    <div class="border-border-tertiary mt-5 border-t pt-4">
      <p class="text-text-tertiary text-sm">
        {$t`No apps can send you notifications yet.`}
      </p>
    </div>
  {/if}
</section>
