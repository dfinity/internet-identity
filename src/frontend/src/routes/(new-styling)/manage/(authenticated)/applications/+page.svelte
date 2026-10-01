<script lang="ts">
  import { SvelteMap, SvelteSet } from "svelte/reactivity";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { currentBrowserId } from "$lib/stores/browser-key.store";
  import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
  import { notificationsEnabledFor } from "$lib/globals";
  import { Trans } from "$lib/components/locale";
  import { formatRelative, t } from "$lib/stores/locale.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { isPushSupported } from "$lib/utils/notifications/pushSubscription";
  import {
    allowApp,
    disallowApp,
    enableNotifications,
  } from "$lib/utils/notifications/enableNotifications";
  import AppRow from "./components/AppRow.svelte";
  import AppDetails from "./components/AppDetails.svelte";
  import { appsFrom, type App } from "./apps";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  const apps = $derived(appsFrom(data.applications));

  // Anywhere else the canister refuses the grant, so a switch there could only fail.
  const canNotify = (origin: string): boolean =>
    $PUSH_NOTIFICATIONS && notificationsEnabledFor(origin);

  const notifying = $derived(
    apps.map(({ origin }) => origin).filter(canNotify),
  );

  // What the user has switched since the canister answered, which wins over it.
  const switched = new SvelteMap<string, boolean>();
  const allowedOf = (app: App): boolean =>
    switched.get(app.origin) ?? app.notificationsAllowed;
  // Per app, so one app's save finishing does not free another's switch mid-save.
  const saving = new SvelteSet<string>();

  // Only an app sign-in through a session gives this browser a key for the identity,
  // and without one it cannot be registered for Web Push. Read ahead, since anything
  // awaited before the permission prompt takes it out of the user's click.
  // Tracked apart from the answer, which is `undefined` for a browser with no key: a
  // switch flipped before the read is in would take that for one.
  let browserId = $state<number | undefined>(undefined);
  let browserIdRead = $state(false);
  $effect(() => {
    void currentBrowserId($authenticatedStore.identityNumber).then((id) => {
      browserId = id;
      browserIdRead = true;
    });
  });

  const setAllowed = async (app: App, next: boolean) => {
    const { actor, identityNumber } = $authenticatedStore;
    const { origin } = app;
    const previous = allowedOf(app);
    switched.set(origin, next);
    saving.add(origin);
    try {
      if (!next) {
        await disallowApp({ identityNumber, origin, actor });
      } else if (!isPushSupported() || browserId === undefined) {
        // This browser cannot be registered, but consent belongs to the identity, so
        // it still reaches the browsers that can show it.
        await allowApp({ identityNumber, origin, actor });
      } else {
        // Nothing is awaited before it, so the permission prompt it opens with stays
        // inside the click. It subscribes this browser before granting, so a refusal
        // at the prompt records nothing.
        const { status } = await enableNotifications({
          identityNumber,
          origin,
          actor,
        });
        if (status !== "enabled") {
          switched.set(origin, previous);
        }
        if (status === "denied") {
          toaster.error({
            title: $t`Notifications are blocked`,
            description: $t`Allow notifications for this site in your browser's settings, then try again.`,
          });
        }
      }
    } catch {
      switched.set(origin, previous);
      toaster.error({
        title: $t`Couldn't save your change. Please try again.`,
        duration: 4000,
      });
    } finally {
      saving.delete(origin);
    }
  };

  const lastVisitedOf = (app: App): string =>
    $formatRelative(new Date(app.lastUsedMillis), { style: "long" });

  let selected = $state<App | undefined>(undefined);
  const dialogTitleId = $props.id();
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Applications`}</h1>
  <p class="text-text-tertiary text-base">
    {#if notifying.length > 0}
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
            lastVisited={lastVisitedOf(app)}
            allowed={canNotify(app.origin) ? allowedOf(app) : undefined}
            showNotifications={notifying.length > 0}
            onOpen={() => (selected = app)}
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
    onClose={() => (selected = undefined)}
    aria-labelledby={dialogTitleId}
  >
    <AppDetails
      titleId={dialogTitleId}
      origin={app.origin}
      canNotify={canNotify(app.origin)}
      allowed={allowedOf(app)}
      busy={saving.has(app.origin) || !browserIdRead}
      onAllowedChange={(next) => void setAllowed(app, next)}
    />
  </Dialog>
{/if}
