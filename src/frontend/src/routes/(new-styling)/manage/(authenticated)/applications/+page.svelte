<script lang="ts">
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { handleError } from "$lib/components/utils/error";
  import {
    allowApp,
    disallowApp,
  } from "$lib/utils/notifications/enableNotifications";
  import AppRow from "./components/AppRow.svelte";
  import AppDetails from "./components/AppDetails.svelte";
  import { appsFrom, type App } from "./apps";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  // Overwritten once a save succeeds. Loading the page again reads what the canister
  // holds.
  let apps = $derived(appsFrom(data.applications));

  // Consent belongs to the identity, so it reaches every browser it registered for
  // notifications, not only this one.
  const saveAllowed = async (origin: string, allowed: boolean) => {
    const { actor, identityNumber } = $authenticatedStore;
    try {
      await (allowed ? allowApp : disallowApp)({
        identityNumber,
        origin,
        actor,
      });
    } catch (error) {
      handleError(error);
      return;
    }
    apps = apps.map((app) =>
      app.origin === origin ? { ...app, notificationsAllowed: allowed } : app,
    );
    selectedOrigin = undefined;
  };

  const allowedOf = (app: App): boolean =>
    $PUSH_NOTIFICATIONS && app.notificationsAllowed;

  let selectedOrigin = $state<string | undefined>(undefined);
  const selected = $derived(
    apps.find(({ origin }) => origin === selectedOrigin),
  );
  const dialogTitleId = $props.id();
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Applications`}</h1>
  <p class="text-text-tertiary text-base">
    {#if $PUSH_NOTIFICATIONS}
      <Trans>Apps you've signed in to. Choose which ones can notify you.</Trans>
    {:else}
      <Trans>Apps you've signed in to.</Trans>
    {/if}
  </p>
</header>

<div class="mt-10 flex max-w-3xl flex-col">
  {#if apps.length > 0}
    <ul
      class="border-border-secondary bg-bg-primary @container/apps overflow-hidden rounded-xl border"
    >
      {#each apps as app, index (app.origin)}
        {#if index > 0}
          <li
            aria-hidden="true"
            class="border-border-tertiary mx-4 border-t"
          ></li>
        {/if}
        <li>
          <AppRow
            origin={app.origin}
            lastUsedMillis={app.lastUsedMillis}
            allowed={allowedOf(app)}
            lastNotifiedMillis={app.lastNotifiedMillis}
            showNotifications={$PUSH_NOTIFICATIONS}
            onManage={() => (selectedOrigin = app.origin)}
          />
        </li>
      {/each}
    </ul>
  {:else}
    <p
      class="border-border-secondary text-text-tertiary rounded-xl border border-dashed p-6 text-center text-sm"
    >
      {$t`Apps you sign in to will show up here.`}
    </p>
  {/if}
</div>

{#if selected !== undefined}
  {@const app = selected}
  <Dialog
    onClose={() => (selectedOrigin = undefined)}
    aria-labelledby={dialogTitleId}
  >
    <AppDetails
      titleId={dialogTitleId}
      origin={app.origin}
      canNotify={$PUSH_NOTIFICATIONS}
      allowed={allowedOf(app)}
      onSave={(allowed) => saveAllowed(app.origin, allowed)}
    />
  </Dialog>
{/if}
