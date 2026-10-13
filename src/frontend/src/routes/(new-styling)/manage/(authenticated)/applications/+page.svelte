<script lang="ts">
  import { invalidateAll } from "$app/navigation";
  import { LayoutGridIcon } from "@lucide/svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
  import { plural, t } from "$lib/stores/locale.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import {
    allowApp,
    disallowApp,
  } from "$lib/utils/notifications/enableNotifications";
  import AppCard from "./components/AppCard.svelte";
  import AppDetails from "./components/AppDetails.svelte";
  import AppSelectCard from "./components/AppSelectCard.svelte";
  import ForgetApps from "./components/ForgetApps.svelte";
  import ForgetBar from "./components/ForgetBar.svelte";
  import { appName, appsFrom, forgetApps, type App } from "./apps";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  // Written ahead of each change, and recomputed from what the canister holds once the
  // change settles and the page data is reloaded.
  let apps = $derived(appsFrom(data.applications));

  // Set while a permission change is in flight, and cleared once the page data is
  // reloaded, so a second change cannot race the first.
  let saving = $derived.by(() => {
    void data;
    return false;
  });

  const allowedOf = (app: App): boolean =>
    $PUSH_NOTIFICATIONS && app.notificationsAllowed;

  // Consent belongs to the identity, so it reaches every browser it registered for
  // notifications, not only this one.
  const saveAllowed = async (origin: string, allowed: boolean) => {
    const { actor, identityNumber } = $authenticatedStore;
    const previous = apps;
    saving = true;
    apps = apps.map((app) =>
      app.origin === origin ? { ...app, notificationsAllowed: allowed } : app,
    );
    try {
      await (allowed ? allowApp : disallowApp)({
        identityNumber,
        origin,
        actor,
      });
    } catch (error) {
      apps = previous;
      handleError(error);
    } finally {
      void invalidateAll();
    }
  };

  let opened = $state<string>();
  const openedApp = $derived(apps.find(({ origin }) => origin === opened));

  let selecting = $state(false);
  let selected = $state<string[]>([]);
  const toggleSelected = (origin: string) => {
    selected = selected.includes(origin)
      ? selected.filter((one) => one !== origin)
      : [...selected, origin];
  };
  const stopSelecting = () => {
    selecting = false;
    selected = [];
  };

  let forgetting = $state<[string, ...string[]]>();

  const forget = async (origins: [string, ...string[]]) => {
    const { actor, identityNumber } = $authenticatedStore;
    // Named before the call: the apps leave the list as soon as it succeeds.
    const name = appName(origins[0]);
    try {
      await forgetApps({ identityNumber, origins, actor });
    } catch (error) {
      handleError(error);
      return;
    } finally {
      void invalidateAll();
    }
    apps = apps.filter(({ origin }) => !origins.includes(origin));
    forgetting = undefined;
    opened = undefined;
    stopSelecting();
    toaster.success({
      title:
        origins.length === 1
          ? $t`${name} has been removed.`
          : $plural(origins.length, {
              one: "1 app has been removed.",
              other: "# apps have been removed.",
            }),
      description:
        origins.length === 1
          ? $t`You've been signed out and it can no longer notify you.`
          : $t`You've been signed out and they can no longer notify you.`,
    });
  };

  const dialogTitleId = $props.id();
</script>

<header class="flex max-w-3xl flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Applications`}</h1>
  <p class="text-text-tertiary text-base">
    {$t`Open or manage apps you've granted permissions.`}
  </p>
</header>

<div class="mt-10 flex max-w-3xl flex-col">
  {#if apps.length > 0}
    <ForgetBar
      {selecting}
      count={selected.length}
      onStart={() => (selecting = true)}
      onCancel={stopSelecting}
      onForget={() => {
        const [first, ...rest] = selected;
        if (first !== undefined) {
          forgetting = [first, ...rest];
        }
      }}
    />
    <!-- Two columns once the list, not the page, is wide enough: the sidebar can narrow it. -->
    <div class="@container/apps">
      <ul class="-m-px grid list-none gap-3 p-px @min-[560px]/apps:grid-cols-2">
        {#each apps as app (app.origin)}
          {@const isSelected = selecting && selected.includes(app.origin)}
          <li
            class={[
              "bg-bg-primary relative flex flex-col overflow-hidden rounded-xl border transition-[border-color,box-shadow] duration-150",
              isSelected
                ? "border-fg-primary ring-fg-primary ring-1"
                : "border-border-secondary shadow-xs",
            ]}
          >
            {#if selecting}
              <AppSelectCard
                origin={app.origin}
                selected={isSelected}
                onToggle={() => toggleSelected(app.origin)}
              />
            {:else}
              <AppCard
                origin={app.origin}
                allowed={allowedOf(app)}
                lastNotifiedMillis={app.lastNotifiedMillis}
                showNotifications={$PUSH_NOTIFICATIONS}
                onManage={() => (opened = app.origin)}
              />
            {/if}
          </li>
        {/each}
      </ul>
    </div>
  {:else}
    <div
      class="border-border-tertiary bg-bg-primary flex flex-col items-center justify-center gap-2 rounded-sm border border-dashed px-6 py-10 text-center"
    >
      <LayoutGridIcon class="text-fg-secondary size-7" aria-hidden="true" />
      <p class="text-text-tertiary text-sm">{$t`No apps yet`}</p>
    </div>
  {/if}
</div>

{#if forgetting !== undefined}
  {@const origins = forgetting}
  <Dialog
    onClose={() => (forgetting = undefined)}
    aria-labelledby={dialogTitleId}
  >
    <ForgetApps
      titleId={dialogTitleId}
      {origins}
      onForget={() => forget(origins)}
      onCancel={() => (forgetting = undefined)}
    />
  </Dialog>
{:else if openedApp !== undefined}
  {@const app = openedApp}
  <Dialog onClose={() => (opened = undefined)} aria-labelledby={dialogTitleId}>
    <AppDetails
      titleId={dialogTitleId}
      origin={app.origin}
      canNotify={$PUSH_NOTIFICATIONS}
      allowed={allowedOf(app)}
      {saving}
      onToggle={(allowed) => saveAllowed(app.origin, allowed)}
      onForget={() => (forgetting = [app.origin])}
    />
  </Dialog>
{/if}
