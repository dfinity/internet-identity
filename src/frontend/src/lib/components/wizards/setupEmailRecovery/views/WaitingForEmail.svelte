<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { onDestroy, onMount } from "svelte";

  interface Props {
    /** Address the user typed at step 1. Shown inline so the user can
     *  glance back at what they typed without re-opening the compose
     *  card. */
    fromAddress: string;
    /** The canister-issued nonce. Surfaced in the troubleshooting
     *  block (after `nudgeAfterMs`) so the user can sanity-check their
     *  sent-folder subject line against the exact value we expect. */
    nonce: string;
    /** Recipient mailbox — `register@<host>` or `recover@<host>`. */
    mailbox: string;
    /** Milliseconds before the body copy switches from a time
     *  estimate to a troubleshooting nudge. Defaults to 60 s; exposed
     *  as a prop so tests can shorten it without sleeping. */
    nudgeAfterMs?: number;
  }

  const {
    fromAddress,
    nonce,
    mailbox,
    nudgeAfterMs = 60_000,
  }: Props = $props();

  // Body copy starts as a time estimate ("10–30 s") and swaps to a
  // troubleshooting nudge once we've been on this view for
  // `nudgeAfterMs`. The spinner stays — the canister can flip
  // terminal at any moment and we don't want to *look* like we've
  // given up. The 60s threshold is a placeholder; once Plausible
  // funnels have a few days of real `Pending → terminal` p90 data,
  // calibrate against it (design doc §8.6).
  let nudgeShown = $state(false);
  let nudgeTimer: ReturnType<typeof setTimeout> | undefined;
  onMount(() => {
    nudgeTimer = setTimeout(() => {
      nudgeShown = true;
    }, nudgeAfterMs);
  });
  onDestroy(() => {
    if (nudgeTimer !== undefined) clearTimeout(nudgeTimer);
  });
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col gap-2">
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Waiting for your email...`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">
      {$t`We're checking for an email from ${fromAddress}.`}
    </p>
  </header>

  <div class="flex items-center justify-center py-8">
    <ProgressRing class="text-fg-primary size-12!" />
  </div>

  {#if nudgeShown}
    <!-- Troubleshooting nudge — replaces the time estimate once
         `nudgeAfterMs` has elapsed. Covers the three failure modes
         we expect at this point: typo'd subject, wrong From: address,
         mail still queued client-side. -->
    <div class="flex flex-col gap-3">
      <p class="text-text-tertiary text-sm">
        {$t`Taking longer than expected. A few things to check:`}
      </p>
      <ul class="text-text-tertiary flex list-disc flex-col gap-2 pl-5 text-sm">
        <li>
          {$t`Confirm the email was sent from ${fromAddress} (not an alias or a different address).`}
        </li>
        <li>
          {$t`Check the subject line is exactly:`}
          <code
            class="bg-bg-secondary text-text-primary mt-1 block rounded px-2 py-1 font-mono text-xs break-all"
            >{nonce}</code
          >
        </li>
        <li>
          {$t`Check your sent folder — some providers queue outbound mail.`}
        </li>
      </ul>
    </div>
  {:else}
    <p class="text-text-tertiary text-sm">
      {$t`This usually takes 10–30 seconds after you send.`}
    </p>
  {/if}

  <!-- Collapsible compose-card reminder. Read-only inline summary so
       the user can glance back at the To/Subject without losing
       place. We deliberately don't repeat the full compose card with
       copy buttons here — the user has already opened the mail app,
       this is the "did I get it right?" recap. If they did get it
       wrong, the dialog X button + re-open issues a fresh nonce
       naturally. -->
  <details class="text-text-tertiary text-sm">
    <summary class="cursor-pointer select-none">
      {$t`Show what to send`}
    </summary>
    <dl
      class="border-border-tertiary mt-3 flex flex-col gap-2 border-t pt-3 font-mono text-xs"
    >
      <div class="flex flex-col gap-0.5">
        <dt
          class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
        >
          {$t`To`}
        </dt>
        <dd class="text-text-primary break-all">{mailbox}</dd>
      </div>
      <div class="flex flex-col gap-0.5">
        <dt
          class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
        >
          {$t`From`}
        </dt>
        <dd class="text-text-primary break-all">{fromAddress}</dd>
      </div>
      <div class="flex flex-col gap-0.5">
        <dt
          class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
        >
          {$t`Subject`}
        </dt>
        <dd class="text-text-primary break-all">{nonce}</dd>
      </div>
    </dl>
  </details>
</div>
