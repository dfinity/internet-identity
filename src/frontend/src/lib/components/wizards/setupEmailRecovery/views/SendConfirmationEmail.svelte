<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { CopyIcon, ExternalLinkIcon, ShieldCheckIcon } from "@lucide/svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { waitFor } from "$lib/utils/utils";
  import type { Path } from "$lib/utils/dnssec";
  import { onDestroy, onMount } from "svelte";

  interface Props {
    /** The canister-issued nonce (e.g. `II-Recovery-1a2b3c4d…`) — also
     *  the email's required `Subject:` value. */
    nonce: string;
    /** Recipient mailbox the user emails to: `register@<host>` or
     *  `recover@<host>`. */
    mailbox: string;
    /** Address the user typed at step 1, shown back so they don't send
     *  the email from the wrong account. */
    fromAddress: string;
    /** Which DNS-verification path the canister will run when the
     *  email arrives — drives the trust-story tooltip on the From row.
     *  "dnssec" when the FE submitted a signed bundle; "doh" when the
     *  canister will fall back to its multi-provider DoH quorum. */
    path: Path;
    /** When `false` (default): compose mode — copy buttons on To/Subject,
     *  shield-tooltip on From, body-row placeholder, "Open in mail app"
     *  + "I've sent the email" buttons below the card.
     *  When `true`: sent mode — card gains a "Waiting to receive…"
     *  status header strip, row values go muted, copy buttons and
     *  shield disappear, body row hides, and the action buttons swap
     *  for a "Having trouble?" collapsible that auto-opens after 60 s. */
    sent?: boolean;
    /** Fired when the user clicks "I've sent the email" in compose
     *  mode. Pure presentation hook — the parent wizard's polling loop
     *  already runs; this just flips the wizard's stage so the rendered
     *  view re-renders with `sent={true}`. Required only when
     *  `sent === false`. */
    onSent?: () => void;
    /** Milliseconds before the "Having trouble?" collapsible auto-opens
     *  in sent mode. Defaults to 60 s; exposed so tests can shorten it
     *  without sleeping. */
    nudgeAfterMs?: number;
  }

  const {
    nonce,
    mailbox,
    fromAddress,
    path,
    sent = false,
    onSent,
    nudgeAfterMs = 60_000,
  }: Props = $props();

  // mailto: link — pre-fills To and Subject, leaves the body empty.
  // Honoured by every native mail client we care about (Apple Mail,
  // Outlook desktop, Thunderbird) and by Gmail / Outlook Web when the
  // user has registered them as default mailto handlers.
  const mailtoHref = $derived(
    `mailto:${mailbox}?subject=${encodeURIComponent(nonce)}`,
  );

  // One-shot "Copied" tooltip per copy target.
  let copiedTo = $state(false);
  let copiedSubject = $state(false);

  // `navigator.clipboard.writeText` rejects on browsers that block
  // clipboard writes outside secure contexts (some embedded iframes,
  // permission-denied prompts on Safari/Firefox). Swallow the failure
  // and skip the "Copied" tooltip rather than throwing an unhandled
  // rejection into devtools — the value the user wanted is still
  // selectable in the UI.
  const copyTo = async () => {
    try {
      await navigator.clipboard.writeText(mailbox);
    } catch {
      return;
    }
    copiedTo = true;
    await waitFor(700);
    copiedTo = false;
  };
  const copySubject = async () => {
    try {
      await navigator.clipboard.writeText(nonce);
    } catch {
      return;
    }
    copiedSubject = true;
    await waitFor(700);
    copiedSubject = false;
  };

  const verificationDescription = $derived(
    path === "dnssec"
      ? $t`Your provider's DKIM key is verified end-to-end via DNSSEC. No trusted intermediaries.`
      : $t`Your provider's DKIM key is verified by quorum consensus across independent DNS resolvers.`,
  );

  // Sent-mode: after `nudgeAfterMs` on this view, force-open the
  // "Having trouble?" collapsible. The user is then either still
  // waiting (we surfaced the troubleshooting bullets without making
  // them dig) or has already clicked it themselves. The canister can
  // still flip terminal at any moment, so we don't change the status
  // strip or hide the spinner — we just elevate the help. 60 s is a
  // placeholder; calibrate against the p90 of Pending → terminal
  // once Plausible funnels have data (design doc §8.6).
  let troubleOpen = $state(false);
  let nudgeTimer: ReturnType<typeof setTimeout> | undefined;
  onMount(() => {
    if (sent) {
      nudgeTimer = setTimeout(() => {
        troubleOpen = true;
      }, nudgeAfterMs);
    }
  });
  onDestroy(() => {
    if (nudgeTimer !== undefined) clearTimeout(nudgeTimer);
  });
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col gap-2">
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Verify your email`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">
      {#if sent}
        {$t`We're waiting for your email.`}
      {:else}
        <Trans>Send the email below to confirm.</Trans>
      {/if}
    </p>
  </header>

  <!-- Pre-filled email card. Two modes:
       - compose (sent=false): To/Subject rows are clickable copy
         targets, From row carries the cryptographic-authenticity
         shield, Body row spells out "anything, blank".
       - sent (sent=true): the card grows a "Waiting to receive…"
         status header strip, copy buttons + shield + body row go
         away, the row values go muted because they're now reference
         info rather than instructions. -->
  <div
    class="bg-bg-primary border-border-secondary flex flex-col overflow-hidden rounded-xl border not-dark:shadow-sm"
  >
    {#if sent}
      <!-- Status header strip. The animated dot uses the existing
           `progress_pulsate` keyframes from ProgressRing so we don't
           have to introduce a new global animation. Kept text-only
           because we don't want a second visual indicator competing
           with the row content below. -->
      <div
        class="border-border-tertiary text-text-primary flex items-center gap-2 border-b px-4 py-3 text-sm font-medium"
      >
        <span
          aria-hidden="true"
          class="bg-fg-primary inline-block size-2 shrink-0 animate-pulse rounded-full"
        ></span>
        <span>{$t`Waiting to receive…`}</span>
      </div>
    {/if}

    {#if sent}
      <div
        class="border-border-tertiary flex flex-col gap-1 border-b px-4 py-3"
      >
        <span
          class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
        >
          {$t`To`}
        </span>
        <span class="text-text-tertiary truncate font-mono text-sm">
          {mailbox}
        </span>
      </div>
    {:else}
      <Tooltip label={$t`Copied to clipboard`} hidden={!copiedTo} manual>
        <button
          type="button"
          onclick={copyTo}
          aria-label={$t`Copy recipient address`}
          class="border-border-tertiary hover:bg-bg-secondary focus-visible:bg-bg-secondary flex w-full flex-col gap-1 border-b px-4 py-3 text-start transition-colors outline-none"
        >
          <span
            class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
          >
            {$t`To`}
          </span>
          <span class="flex items-center gap-2">
            <span class="text-text-primary grow truncate font-mono text-sm">
              {mailbox}
            </span>
            <CopyIcon class="text-fg-tertiary size-4 shrink-0" />
          </span>
        </button>
      </Tooltip>
    {/if}

    <div class="border-border-tertiary flex flex-col gap-1 border-b px-4 py-3">
      <span
        class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
      >
        {$t`From`}
      </span>
      <span class="flex items-center gap-2">
        <span
          class={[
            "grow truncate font-mono text-sm",
            sent ? "text-text-tertiary" : "text-text-primary",
          ]}
        >
          {fromAddress}
        </span>
        {#if !sent}
          <Tooltip
            label={$t`Cryptographically authentic`}
            description={verificationDescription}
            direction="up"
            align="end"
            offset="0rem"
            class="max-w-80"
          >
            <button
              type="button"
              class="btn btn-tertiary btn-icon text-fg-success-primary -me-1.5 size-7! shrink-0 !cursor-default !rounded-full"
              aria-label={$t`Cryptographically authentic`}
            >
              <ShieldCheckIcon class="size-4" aria-hidden="true" />
            </button>
          </Tooltip>
        {/if}
      </span>
    </div>

    {#if sent}
      <div class="flex flex-col gap-1 px-4 py-3">
        <span
          class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
        >
          {$t`Subject`}
        </span>
        <span class="text-text-tertiary truncate font-mono text-sm">
          {nonce}
        </span>
      </div>
    {:else}
      <Tooltip label={$t`Copied to clipboard`} hidden={!copiedSubject} manual>
        <button
          type="button"
          onclick={copySubject}
          aria-label={$t`Copy subject`}
          class="border-border-tertiary hover:bg-bg-secondary focus-visible:bg-bg-secondary flex w-full flex-col gap-1 border-b px-4 py-3 text-start transition-colors outline-none"
        >
          <span
            class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
          >
            {$t`Subject`}
          </span>
          <span class="flex items-center gap-2">
            <span class="text-text-primary grow truncate font-mono text-sm">
              {nonce}
            </span>
            <CopyIcon class="text-fg-tertiary size-4 shrink-0" />
          </span>
        </button>
      </Tooltip>

      <div class="flex flex-col gap-1 px-4 py-3">
        <span
          class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
        >
          {$t`Body`}
        </span>
        <span class="text-text-tertiary text-sm italic">
          {$t`(anything, leave it blank)`}
        </span>
      </div>
    {/if}
  </div>

  {#if sent}
    <!-- "Having trouble?" — same collapsible affordance the
         UnsupportedDomain view uses for "Why isn't this supported?",
         so users recognise the pattern. Force-opens after
         `nudgeAfterMs` (60 s) so a stuck user doesn't have to dig. -->
    <details
      bind:open={troubleOpen}
      class="border-border-secondary rounded-xl border"
    >
      <summary
        class="text-text-primary hover:bg-bg-secondary focus-visible:bg-bg-secondary flex cursor-pointer items-center gap-2 rounded-xl px-4 py-3 text-sm font-medium transition-colors outline-none select-none"
      >
        {$t`Having trouble?`}
      </summary>
      <div class="border-border-tertiary border-t px-4 py-3">
        <ul
          class="text-text-tertiary flex list-disc flex-col gap-2 pl-5 text-sm"
        >
          <li>
            {$t`Make sure you sent the email from ${fromAddress} — aliases and forwarders won't work.`}
          </li>
          <li>
            {$t`Make sure the subject line is exactly:`}
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
    </details>
  {:else}
    <!-- Static lifetime hint. The actual TTL lives in
         `email_recovery::CHALLENGE_TTL_SECS` (30 minutes). We don't
         run a ticking countdown — the status-poll surfaces `Expired`
         via `FailedView` when the user is out of time, which is the
         authoritative source. -->
    <p class="text-text-tertiary text-xs">
      {$t`This email link expires in about 30 minutes.`}
    </p>

    <div class="flex flex-col gap-2">
      <a class="btn btn-primary btn-lg" href={mailtoHref}>
        <ExternalLinkIcon class="size-5" />
        <span>{$t`Open in mail app`}</span>
      </a>
      <!-- Presentation-only secondary action. The polling loop in the
           parent wizard already runs from the moment `prepare_add`
           returned, so the canister can flip terminal without the user
           ever clicking this — notably the DoH path. This button just
           re-renders the same component with `sent={true}` so the
           screen visibly responds to the user's physical action of
           sending the mail. -->
      <button type="button" class="btn btn-secondary btn-lg" onclick={onSent}>
        {$t`I've sent the email`}
      </button>
    </div>
  {/if}
</div>
