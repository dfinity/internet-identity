<script lang="ts">
  import { onMount } from "svelte";
  import { t } from "$lib/stores/locale.store";
  import {
    CircleCheckIcon,
    CircleAlertIcon,
    CircleXIcon,
    CircleMinusIcon,
  } from "@lucide/svelte";
  import type { PageProps } from "./$types";
  import type {
    PostboxEmail,
    DkimCheck,
    DkimCheckName,
    DkimCheckStatus,
    DkimVerificationStatus,
  } from "$lib/generated/internet_identity_types";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { toaster } from "$lib/components/utils/toaster";
  import {
    isPushSupported,
    isCurrentlySubscribed,
    subscribeToPush,
    unsubscribeFromPush,
    postLatestEmailToServiceWorker,
  } from "$lib/utils/pushNotifications";

  const { data }: PageProps = $props();

  let selectedIndex = $state(0);
  // `pollingEmails` is set by the polling loop below so new emails appear
  // without requiring re-authentication. When it's undefined we fall back to
  // the list the layout loader provided, which keeps the initial render
  // correct and also lets layout reloads (e.g. after navigation) win until
  // the next poll completes.
  let pollingEmails: PostboxEmail[] | undefined = $state(undefined);
  const emails: PostboxEmail[] = $derived(pollingEmails ?? data.postboxEmails);
  const selectedEmail: PostboxEmail | undefined = $derived(
    emails[selectedIndex],
  );

  /**
   * Stable identity for an email used to preserve the user's selection across
   * refreshes. `get_postbox` returns emails newest-first; when a new email
   * arrives the index of every previously-visible email shifts by one, and
   * without this lookup the detail pane would silently switch to a different
   * message while the user was reading.
   */
  const emailKey = (email: PostboxEmail): string =>
    `${email.sender}\u0000${email.subject}\u0000${email.body}`;

  // Push notification state. `pushSupported` is known immediately; the
  // subscription state needs an async check against the service worker
  // registration.
  const pushSupported = isPushSupported();
  let isPushSubscribed = $state(false);
  let togglingPush = $state(false);

  /** Poll the canister for new emails this often. */
  const POSTBOX_POLL_INTERVAL_MS = 5_000;

  /**
   * Jumps to the newest email and scrolls the detail pane into view. Called
   * on mount when the page is opened with `#email-detail` (e.g. via a
   * clicked push notification) and in response to `SHOW_LATEST_EMAIL` from
   * the service worker when an already-open tab is focused.
   */
  const showLatestEmail = (): void => {
    selectedIndex = 0;
    // Defer to next tick so the selection change has rendered before we
    // scroll to the detail element.
    requestAnimationFrame(() => {
      document
        .getElementById("email-detail")
        ?.scrollIntoView({ behavior: "smooth", block: "start" });
    });
  };

  onMount(() => {
    let cancelled = false;
    let inFlight = false;

    const refreshPostbox = async (): Promise<void> => {
      if (inFlight || cancelled) return;
      // No reason to burn cycles polling while the tab is hidden — we
      // refresh immediately when it becomes visible again (below).
      if (document.visibilityState !== "visible") return;
      inFlight = true;
      try {
        const authenticated = $authenticatedStore;
        const fresh = await authenticated.actor.get_postbox(
          authenticated.identityNumber,
        );
        if (cancelled) return;

        const currentSelected = emails[selectedIndex];
        const previousKey =
          currentSelected !== undefined ? emailKey(currentSelected) : undefined;
        pollingEmails = fresh;
        if (previousKey !== undefined) {
          const newIndex = fresh.findIndex((e) => emailKey(e) === previousKey);
          // If the selected email was evicted (list is capped server-side),
          // snap back to the newest.
          selectedIndex = newIndex === -1 ? 0 : newIndex;
        }

        // PoC side-channel for push notifications: keep the service
        // worker's latest-email cache warm so the next `push` event can
        // show real sender/subject instead of a generic fallback. Replaced
        // by RFC 8291 payload encryption later.
        if (fresh[0] !== undefined) {
          void postLatestEmailToServiceWorker(fresh[0]);
        }
      } catch {
        // Transient network or canister error — keep the current list and
        // try again on the next tick.
      } finally {
        inFlight = false;
      }
    };

    const interval = window.setInterval(
      refreshPostbox,
      POSTBOX_POLL_INTERVAL_MS,
    );
    const onVisibilityChange = (): void => {
      if (document.visibilityState === "visible") {
        void refreshPostbox();
      }
    };
    document.addEventListener("visibilitychange", onVisibilityChange);

    // Seed the service worker's latest-email cache from whatever the
    // layout loader already has, so even a push that arrives before the
    // first poll completes has metadata to show.
    if (data.postboxEmails[0] !== undefined) {
      void postLatestEmailToServiceWorker(data.postboxEmails[0]);
    }

    // If this page was opened via a notification click, focus the newest
    // email and scroll its body into view. The browser's hash auto-scroll
    // can trigger first, but we also force `selectedIndex = 0` here so the
    // user lands on the email that actually caused the notification.
    if (window.location.hash === "#email-detail") {
      showLatestEmail();
    }

    // If the tab was already open and the service worker focused it, it
    // posts `SHOW_LATEST_EMAIL` so we can reset selection + scroll.
    const onSwMessage = (event: MessageEvent): void => {
      // Only trust messages from our own service worker. `event.source`
      // should be a `ServiceWorker` registered at our origin; messages
      // from anything else get dropped on the floor.
      if (event.origin !== window.location.origin) return;
      if (event.data?.type === "SHOW_LATEST_EMAIL") {
        showLatestEmail();
      }
    };
    navigator.serviceWorker?.addEventListener("message", onSwMessage);

    // Push notification subscription state lookup — one-shot.
    if (pushSupported) {
      void isCurrentlySubscribed()
        .then((subscribed) => {
          if (!cancelled) isPushSubscribed = subscribed;
        })
        .catch(() => {
          // Treat as not subscribed; the toggle reflects that.
        });
    }

    return () => {
      cancelled = true;
      window.clearInterval(interval);
      document.removeEventListener("visibilitychange", onVisibilityChange);
      navigator.serviceWorker?.removeEventListener("message", onSwMessage);
    };
  });

  const handleTogglePush = async (
    event: Event & { currentTarget: HTMLInputElement },
  ): Promise<void> => {
    const wantOn = event.currentTarget.checked;
    // Guard against re-entry while the async work is in flight.
    if (togglingPush) {
      event.currentTarget.checked = isPushSubscribed;
      return;
    }
    togglingPush = true;

    const authenticated = $authenticatedStore;
    try {
      if (wantOn) {
        const subscription = await subscribeToPush(
          authenticated.actor,
          authenticated.identityNumber,
        );
        if (subscription === null) {
          // User denied the browser permission prompt.
          isPushSubscribed = false;
          event.currentTarget.checked = false;
          toaster.info({
            title: $t`Notifications not enabled`,
            description: $t`Your browser blocked the permission prompt.`,
          });
        } else {
          isPushSubscribed = true;
        }
      } else {
        await unsubscribeFromPush(
          authenticated.actor,
          authenticated.identityNumber,
        );
        isPushSubscribed = false;
      }
    } catch (err) {
      event.currentTarget.checked = isPushSubscribed;
      toaster.error({
        title: $t`Notifications failed`,
        description: err instanceof Error ? err.message : String(err),
      });
    } finally {
      togglingPush = false;
    }
  };

  const checkLabel = (name: DkimCheckName): string => {
    if ("DkimSignaturePresent" in name)
      return $t`DKIM-Signature header present`;
    if ("SignatureParsed" in name) return $t`Signature parsed`;
    if ("AlgorithmSupported" in name) return $t`Algorithm supported`;
    if ("RequiredHeadersSigned" in name) return $t`Required headers signed`;
    if ("BodyHashValid" in name) return $t`Body hash valid`;
    if ("PublicKeyFetched" in name) return $t`Public key fetched via DNS`;
    if ("SignatureValid" in name) return $t`RSA signature valid`;
    return "";
  };

  const isPass = (status: DkimCheckStatus): boolean => "Pass" in status;
  const isFail = (status: DkimCheckStatus): boolean => "Fail" in status;

  const getChecks = (
    status: DkimVerificationStatus,
  ): DkimCheck[] | undefined => {
    if ("Verified" in status) return status.Verified.checks;
    if ("Unverified" in status) return status.Unverified.checks;
    return undefined;
  };
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">
    {$t`Postbox`}
  </h1>
  <p class="text-text-tertiary text-base">
    {$t`Emails received for your identity.`}
  </p>
  {#if pushSupported}
    <div class="mt-2 flex items-center gap-3">
      <Toggle
        size="sm"
        checked={isPushSubscribed}
        disabled={togglingPush}
        onchange={handleTogglePush}
        label={$t`Email notifications`}
        hint={$t`Get notified on this device when a new email arrives.`}
      />
    </div>
  {/if}
</header>

<div class="mt-10 flex flex-col gap-6 md:flex-row md:gap-8">
  <!-- Email list -->
  <div
    class="border-border-secondary flex w-full flex-col overflow-hidden rounded-xl border md:w-80 md:shrink-0"
  >
    <ul class="divide-border-secondary divide-y">
      {#each emails as email, i}
        <li class="contents">
          <button
            class={[
              "flex w-full cursor-pointer flex-col gap-1 px-4 py-3 text-left transition-colors",
              i === selectedIndex
                ? "bg-bg-active"
                : "hover:bg-bg-primary_hover",
            ]}
            onclick={() => (selectedIndex = i)}
          >
            <span class="text-text-primary truncate text-sm font-medium">
              {email.subject || $t`(no subject)`}
            </span>
            <span class="text-text-tertiary flex items-center gap-1 text-xs">
              <span class="truncate">{email.sender}</span>
              {#if email.dkim_status[0] !== undefined}
                {@const status = email.dkim_status[0]}
                {#if "Verified" in status}
                  <CircleCheckIcon
                    title={$t`DKIM verified`}
                    class="text-fg-success-primary size-4 shrink-0"
                  />
                {:else if "Pending" in status}
                  <CircleMinusIcon
                    title={$t`Verifying...`}
                    class="text-text-tertiary size-4 shrink-0"
                  />
                {:else}
                  <CircleAlertIcon
                    title={$t`Not verified`}
                    class="text-fg-warning-primary size-4 shrink-0"
                  />
                {/if}
              {/if}
            </span>
          </button>
        </li>
      {/each}
    </ul>
  </div>

  <!-- Email content -->
  <div
    id="email-detail"
    class="bg-bg-secondary flex min-h-64 flex-1 scroll-mt-4 flex-col rounded-xl p-6"
  >
    {#if selectedEmail}
      <div class="mb-4 flex flex-col gap-1">
        <h2 class="text-text-primary text-lg font-medium">
          {selectedEmail.subject || $t`(no subject)`}
        </h2>
        <p class="text-text-tertiary text-sm">
          {$t`From:`}
          {selectedEmail.sender}
        </p>
        <p class="text-text-tertiary text-sm">
          {$t`To:`}
          {selectedEmail.recipient}
        </p>
        {#if selectedEmail.dkim_status[0] !== undefined}
          {@const status = selectedEmail.dkim_status[0]}
          {@const checks = getChecks(status)}
          <div class="mt-2 flex items-center gap-2">
            {#if "Verified" in status}
              <CircleCheckIcon class="text-fg-success-primary size-4" />
              <span class="text-fg-success-primary text-sm">
                {$t`DKIM verified`}
              </span>
            {:else if "Pending" in status}
              <CircleMinusIcon class="text-text-tertiary size-4" />
              <span class="text-text-tertiary text-sm">
                {$t`Verifying...`}
              </span>
            {:else}
              <CircleAlertIcon class="text-fg-warning-primary size-4" />
              <span class="text-fg-warning-primary text-sm">
                {$t`Not verified`}
              </span>
            {/if}
          </div>
          {#if checks !== undefined}
            <div
              class="bg-bg-tertiary border-border-secondary mt-2 flex max-h-80 w-80 flex-col overflow-y-auto rounded-lg border p-3"
            >
              <span class="text-text-primary mb-2 text-xs font-semibold">
                {$t`Verification details`}
              </span>
              <ul class="flex flex-col gap-1.5">
                {#each checks as check}
                  <li class="flex flex-col gap-0.5">
                    <div class="flex items-center gap-1.5">
                      {#if isPass(check.status)}
                        <CircleCheckIcon
                          class="text-fg-success-primary size-3.5 shrink-0"
                        />
                      {:else if isFail(check.status)}
                        <CircleXIcon
                          class="text-fg-danger-primary size-3.5 shrink-0"
                        />
                      {:else}
                        <CircleMinusIcon
                          class="text-text-disabled size-3.5 shrink-0"
                        />
                      {/if}
                      <span
                        class={[
                          "text-xs",
                          isPass(check.status)
                            ? "text-text-primary"
                            : isFail(check.status)
                              ? "text-fg-danger-primary"
                              : "text-text-disabled",
                        ]}
                      >
                        {checkLabel(check.name)}
                      </span>
                    </div>
                    {#if check.detail[0] !== undefined}
                      <span
                        class="text-text-tertiary ml-5 text-[11px] break-words"
                      >
                        {check.detail[0]}
                      </span>
                    {/if}
                  </li>
                {/each}
              </ul>
            </div>
          {/if}
        {/if}
      </div>
      <div class="border-border-secondary border-t pt-4">
        <pre
          class="text-text-secondary text-sm break-words whitespace-pre-wrap">{selectedEmail.body}</pre>
      </div>
    {:else}
      <p class="text-text-tertiary text-sm">
        {$t`No email selected.`}
      </p>
    {/if}
  </div>
</div>
