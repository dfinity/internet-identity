<script lang="ts">
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { Trans } from "$lib/components/locale";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import { currentBrowserId } from "$lib/stores/browser-key.store";
  import { describeBrowser } from "$lib/stores/channelHandlers/describeBrowser";
  import type { BrowserDescription } from "$lib/generated/internet_identity_types";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import DeviceRow from "./components/DeviceRow.svelte";
  import GroupHeading from "./components/GroupHeading.svelte";
  import {
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
  let thisBrowserId = $state<number | undefined>(undefined);
  $effect(() => {
    void currentBrowserId($authenticatedStore.identityNumber).then(
      (id) => (thisBrowserId = id),
    );
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

  // A browser with no record still gets a row, so the page never renders empty for
  // someone reading it from a browser it would otherwise not know about. Given the
  // timestamps of a browser that has just arrived, so it heads its own group.
  const unrecorded = $derived<Browser | undefined>(
    stored.some((browser) => browser.isCurrent) || thisDescription === undefined
      ? undefined
      : {
          id: -1,
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
    browser.id === -1
      ? "none"
      : signingOut === browser.id
        ? "signing-out"
        : signedOut.includes(browser.id) || isSignedOut(browser, now)
          ? "signed-out"
          : "sign-out";

  const lastUsedOf = (browser: Browser): string =>
    browser.id === -1
      ? $t`Never`
      : $formatRelative(new Date(browser.lastUsedMillis), { style: "long" });
  const firstSeenOf = (browser: Browser): string =>
    browser.id === -1
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
  {:else}
    <div
      class="border-border-secondary text-text-tertiary rounded-xl border border-dashed p-6 text-center text-sm"
    >
      <Trans>No browsers are signed in to apps with this identity.</Trans>
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
