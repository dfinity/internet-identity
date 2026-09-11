<script lang="ts">
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { Trans } from "$lib/components/locale";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import { currentBrowserId } from "$lib/stores/browser-key.store";
  import { describeBrowser } from "$lib/utils/describeBrowser";
  import type { BrowserDescription } from "$lib/generated/internet_identity_types";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import DeviceRow from "./components/DeviceRow.svelte";
  import GroupHeading from "./components/GroupHeading.svelte";
  import {
    NO_RECORD_ID,
    fromCanisterBrowsers,
    groupBrowsers,
    isSignedOut,
    lastUsedAgeMillis,
    brandNameOf,
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
  // When this browser first saw the identity, kept locally because the canister cannot
  // know it: its own record only begins at the first app sign-in. Where it does hold a
  // record, that wins — so the date jumps forward once, which is the compromise taken
  // over showing nothing at all until then.
  const firstSeenHere = $derived(
    $lastUsedIdentitiesStore.identities[
      $authenticatedStore.identityNumber.toString()
    ]?.firstSeenTimestampMillis ?? now,
  );

  const unrecorded = $derived<Browser | undefined>(
    !browserIdRead ||
      stored.some((browser) => browser.isCurrent) ||
      thisDescription === undefined
      ? undefined
      : {
          id: NO_RECORD_ID,
          name: nameOf(thisDescription),
          description: thisDescription,
          createdAtMillis: firstSeenHere,
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

  // Nothing to offer on the browser being read from: signing it out here would end the
  // sessions of the page you are looking at, and calling it "Signed out" would be a
  // claim about the one browser this page cannot make one about — it is in use.
  const actionFor = (browser: Browser) =>
    browser.isCurrent
      ? "none"
      : signingOut === browser.id
        ? "signing-out"
        : signedOut.includes(browser.id) || isSignedOut(browser, now)
          ? "signed-out"
          : "sign-out";

  // "Right now" is the browser reading the page, which is in use by definition, and
  // any browser whose stamp is younger than one grain — the record cannot tell those
  // apart, and on a page for spotting a browser you do not recognise, reading as in use
  // is the safe direction to be wrong in. Worded as the access methods and recovery
  // pages word theirs.
  const lastUsedOf = (browser: Browser): string => {
    if (browser.isCurrent) return $t`Right now`;
    const age = lastUsedAgeMillis(browser, now);
    return age === undefined
      ? $t`Right now`
      : $formatRelative(new Date(now - age), { style: "long" });
  };
  const firstSeenOf = (browser: Browser): string =>
    $formatDate(new Date(browser.createdAtMillis), {
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
        description: $t`${browser.name} no longer has access to your apps.`,
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
    <!-- The column tracks live on the card, and every section and row below takes them
         with `grid-cols-subgrid`. So a column is as wide as the widest content in any
         row of the whole card, in any language, rather than a number chosen for the
         English strings — and the platform groups line up with each other, which they
         would not if each owned its own tracks.

         The card is also the container the rows query. That works because it defines
         tracks rather than inheriting them: `container-type` imposes size containment,
         which forbids an element from being a *subgrid*, so no element below may be
         both. -->
    <div
      class="border-border-secondary bg-bg-primary @container/list grid grid-cols-[1fr_auto_auto_auto] gap-x-10 overflow-hidden rounded-xl border px-4"
    >
      {#each groups as group, groupIndex (group.platform)}
        {#if groupIndex > 0}
          <!-- Full width rather than inset, unlike the rule between rows of one group:
               this is the boundary between two groups. The card's own padding is undone
               for it. -->
          <div
            aria-hidden="true"
            class="border-border-tertiary col-span-4 -mx-4 border-t"
          ></div>
        {/if}
        <section class="col-span-4 grid grid-cols-subgrid">
          <!-- Outdented so the heading's own padding places it, rather than adding to
               the card's. -->
          <div class="col-span-4 -mx-4">
            <GroupHeading
              kind={group.kind}
              platform={group.platform}
              count={group.browsers.length}
            />
          </div>
          <ul class="col-span-4 grid grid-cols-subgrid">
            {#each group.browsers as browser, index (browser.id)}
              <!-- Inset rule between rows of one group, so it reads as a divided group
                   rather than as the boundary between two. Drawn as its own element:
                   indenting the row to inset the rule moved the row with it. -->
              {#if index > 0}
                <li
                  aria-hidden="true"
                  class="border-border-tertiary col-span-4 border-t @2xl/list:ms-6"
                ></li>
              {/if}
              <li class="col-span-4 grid grid-cols-subgrid">
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
    <!-- The title names which browser, the button what happens to it: "Sign out" alone
         is the ambiguity this dialog exists to resolve, and repeating the scope in both
         would leave neither saying which row was clicked. -->
    <div class="flex flex-col gap-5 p-1">
      <h2 class="text-text-primary text-2xl font-medium">
        {$t`Sign out ${brandNameOf(target.description)}?`}
      </h2>

      <p class="text-text-tertiary text-base text-pretty">
        {$t`You can sign in again at any time.`}
      </p>

      <div class="flex flex-col gap-3">
        <button class="btn btn-primary btn-lg w-full" onclick={confirmSignOut}>
          {$t`Sign out of all apps`}
        </button>
        <button
          class="btn btn-tertiary btn-lg w-full"
          onclick={() => (confirming = undefined)}
        >
          {$t`Cancel`}
        </button>
      </div>
    </div>
  </Dialog>
{/if}
