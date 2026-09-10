<script lang="ts">
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { Trans } from "$lib/components/locale";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import { currentBrowserId } from "$lib/stores/browser-key.store";
  import { describeBrowser } from "$lib/utils/describeBrowser";
  import type { BrowserDescription } from "$lib/generated/internet_identity_types";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import DeviceRow from "./components/DeviceRow.svelte";
  import GroupHeading from "./components/GroupHeading.svelte";
  import {
    NO_RECORD_ID,
    fromCanisterBrowsers,
    groupBrowsers,
    isSignedOut,
    nameOf,
    signOutBrowser,
    type Browser,
  } from "./browsers";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  // Read from this browser's own key record rather than from the canister, which has no
  // way to tell which browser is asking: `identity_info` is signed by an access method.
  // `thisBrowserId` stays `undefined` both before the read finishes and when there is no
  // record, so the two are tracked apart: without that, a browser that does have a record
  // renders twice for as long as the read takes — once as the synthetic row below, once
  // as its own.
  let thisBrowserId = $state<number | undefined>(undefined);
  let browserIdRead = $state(false);
  $effect(() => {
    void currentBrowserId($authenticatedStore.identityNumber).then((id) => {
      thisBrowserId = id;
      browserIdRead = true;
    });
  });

  // What this browser is, resolved locally. Needed even when the canister holds no record
  // for it — a browser that has signed in to Internet Identity but never to an app still
  // belongs on this page, and naming it takes no canister data.
  let thisDescription = $state<BrowserDescription | undefined>(undefined);
  $effect(() => {
    void describeBrowser().then((d) => (thisDescription = d));
  });

  const now = Date.now();

  const stored = $derived(
    fromCanisterBrowsers(data.identityInfo.browsers, thisBrowserId),
  );

  // The browser being read from is always on this page, whether or not the canister
  // holds a record for it: it has signed in to Internet Identity, which is how this page
  // is on screen, and it just has not signed in to an app yet. It reads as signed out,
  // because it is, and describing it takes no canister data.
  const unrecorded = $derived<Browser | undefined>(
    !browserIdRead ||
      stored.some((browser) => browser.isCurrent) ||
      thisDescription === undefined
      ? undefined
      : {
          id: NO_RECORD_ID,
          name: nameOf(thisDescription),
          description: thisDescription,
          createdAtMillis: now,
          lastUsedMillis: now,
          sessionCount: 0,
          isCurrent: true,
        },
  );

  const groups = $derived(
    groupBrowsers(
      unrecorded === undefined ? stored : [unrecorded, ...stored],
      now,
    ),
  );

  let signedOut = $state<number[]>([]);
  let signingOut = $state<number | undefined>(undefined);
  let confirming = $state<Browser | undefined>(undefined);

  const actionFor = (browser: Browser) =>
    signingOut === browser.id
      ? "signing-out"
      : signedOut.includes(browser.id) || isSignedOut(browser, now)
        ? "signed-out"
        : "sign-out";

  // A browser with no record has no timestamps to format: it has never signed in to an
  // app, and it arrived just now as far as this page can tell.
  const lastUsedOf = (browser: Browser): string =>
    browser.id === NO_RECORD_ID
      ? $t`Never`
      : $formatRelative(new Date(browser.lastUsedMillis), { style: "long" });
  const firstSeenOf = (browser: Browser): string =>
    browser.id === NO_RECORD_ID
      ? $t`Now`
      : $formatDate(new Date(browser.createdAtMillis), {
          month: "short",
          day: "numeric",
        });

  const confirmSignOut = async () => {
    const browser = confirming;
    if (browser === undefined) {
      return;
    }
    confirming = undefined;
    signingOut = browser.id;
    try {
      await signOutBrowser(
        $authenticatedStore.actor,
        $authenticatedStore.identityNumber,
        browser.id,
      );
      signedOut = [...signedOut, browser.id];
      toaster.success({
        title: $t`Signed out of all apps`,
        description: browser.isCurrent
          ? $t`This browser no longer has access to your apps.`
          : $t`${browser.name} no longer has access to your apps.`,
      });
    } catch (error) {
      toaster.error({
        title: $t`Couldn't sign this browser out`,
        description: error instanceof Error ? error.message : undefined,
      });
    } finally {
      signingOut = undefined;
    }
  };
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Devices`}</h1>
  <p class="text-text-tertiary text-base">
    <Trans>See where you're signed in to apps and sign out remotely.</Trans>
  </p>
</header>

<div class="mt-10 flex max-w-3xl flex-col gap-6">
  <!-- No empty state: this browser is always one of the rows, so the only moment there
       is nothing to draw is before it has described itself. -->
  {#if groups.length > 0}
    <div
      class="border-border-secondary bg-bg-primary flex flex-col overflow-hidden rounded-xl border"
    >
      {#each groups as group, groupIndex (group.platform)}
        <section
          class={groupIndex > 0 ? "border-border-tertiary border-t" : ""}
        >
          <GroupHeading
            kind={group.kind}
            platform={group.platform}
            count={group.browsers.length}
          />
          <ul class="flex flex-col">
            {#each group.browsers as browser, index (browser.id)}
              <!-- Inset rule between rows of one group, so it reads as a divided group
                   rather than as the boundary between two. -->
              <li
                class={index > 0 ? "border-border-tertiary ml-4 border-t" : ""}
              >
                <DeviceRow
                  description={browser.description}
                  lastUsed={lastUsedOf(browser)}
                  firstSeen={firstSeenOf(browser)}
                  isCurrent={browser.isCurrent}
                  action={actionFor(browser)}
                  onSignOut={() => (confirming = browser)}
                />
              </li>
            {/each}
          </ul>
        </section>
      {/each}
    </div>
  {/if}

  <p class="text-text-tertiary text-sm">
    <Trans>
      Don't recognize a browser? Sign it out, then
      <a
        href="/manage/access"
        class="text-text-primary font-semibold hover:underline focus-visible:underline"
        >review your access methods</a
      >.
    </Trans>
  </p>
</div>

{#if confirming !== undefined}
  {@const target = confirming}
  <Dialog onClose={() => (confirming = undefined)} width="wider">
    <div class="flex flex-col gap-5 p-1">
      <FeaturedIcon size="lg" variant="warning" class="self-start">
        <TriangleAlertIcon class="size-6" />
      </FeaturedIcon>

      <h2 class="text-text-primary text-2xl font-medium">
        {$t`Sign out of all apps?`}
      </h2>

      <p class="text-text-tertiary text-base text-pretty">
        {#if target.isCurrent}
          <Trans>
            Every app you opened from this browser will ask you to sign in
            again. You'll stay signed in to Internet Identity here.
          </Trans>
        {:else}
          {$t`${target.name} will lose access to all apps signed in with this identity.`}
        {/if}
      </p>

      <button class="btn btn-primary btn-lg w-full" onclick={confirmSignOut}>
        {$t`Sign out of all apps`}
      </button>
    </div>
  </Dialog>
{/if}
