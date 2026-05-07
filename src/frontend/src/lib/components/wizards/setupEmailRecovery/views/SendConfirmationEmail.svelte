<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import {
    MailIcon,
    CopyIcon,
    Loader2Icon,
  } from "@lucide/svelte";
  import { onMount, onDestroy } from "svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import Steps from "$lib/components/wizards/createRecoveryPhrase/components/Steps.svelte";
  import { waitFor } from "$lib/utils/utils";

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
    /** Unix-ns timestamp at which the canister-side challenge expires. */
    expiresAt: bigint;
    /** Wizard heading prefix — "Add email recovery" for setup, "Recover
     *  with email" for recovery. */
    title: string;
  }

  const { nonce, mailbox, fromAddress, expiresAt, title }: Props = $props();

  // Live countdown to expiration. Canister gives us 30 minutes; we
  // tick every second so the UI doesn't lie about how much time is
  // left.
  let now = $state(Date.now());
  let timer: ReturnType<typeof setInterval> | undefined;
  onMount(() => {
    timer = setInterval(() => {
      now = Date.now();
    }, 1_000);
  });
  onDestroy(() => {
    if (timer) clearInterval(timer);
  });
  const expiresAtMs = $derived(Number(expiresAt / BigInt(1_000_000)));
  const remainingSecs = $derived(
    Math.max(0, Math.floor((expiresAtMs - now) / 1_000)),
  );
  const remainingLabel = $derived(
    remainingSecs >= 60
      ? `${Math.floor(remainingSecs / 60)}:${String(remainingSecs % 60).padStart(2, "0")}`
      : `${remainingSecs}s`,
  );

  // mailto: link — pre-fills To and Subject, leaves the body empty.
  // Honoured by every native mail client we care about (Apple Mail,
  // Outlook desktop, Thunderbird) and by Gmail / Outlook Web when the
  // user has registered them as default mailto handlers.
  const mailtoHref = $derived(
    `mailto:${mailbox}?subject=${encodeURIComponent(nonce)}`,
  );

  // One-shot "Copied" tooltip per copy target. Same pattern as
  // ContinueOnNewDevice.svelte: navigator.clipboard, flip flag, wait
  // ~700 ms, flip back.
  let copiedTo = $state(false);
  let copiedSubject = $state(false);

  const copyTo = async () => {
    await navigator.clipboard.writeText(mailbox);
    copiedTo = true;
    await waitFor(700);
    copiedTo = false;
  };
  const copySubject = async () => {
    await navigator.clipboard.writeText(nonce);
    copiedSubject = true;
    await waitFor(700);
    copiedSubject = false;
  };
</script>

<div class="flex flex-col gap-6">
  <div class="my-2"><Steps total={3} current={2} /></div>

  <header class="flex flex-col gap-2">
    <p class="text-text-tertiary text-xs font-medium uppercase tracking-wide">
      {title} — {$t`step 2 of 3`}
    </p>
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Send your confirmation email`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">
      <Trans>
        Open your inbox and send the email below — we're watching for it.
      </Trans>
    </p>
  </header>

  <!-- The pre-filled email card. Each row is a label + value; To and
       Subject have whole-row copy buttons matching the
       ContinueOnNewDevice pattern. -->
  <div
    class="bg-bg-primary border-border-secondary flex flex-col rounded-xl border not-dark:shadow-sm"
  >
    <div
      class="flex flex-col gap-2 border-b border-border-tertiary px-4 py-3 sm:flex-row sm:items-center sm:gap-3"
    >
      <span
        class="text-text-tertiary w-16 shrink-0 text-xs font-semibold uppercase"
      >
        {$t`To`}
      </span>
      <Tooltip
        label={$t`Copied to clipboard`}
        hidden={!copiedTo}
        manual
        class="grow"
      >
        <button
          class="btn btn-secondary btn-sm w-full justify-between font-mono"
          onclick={copyTo}
          aria-label={$t`Copy recipient`}
        >
          <span class="truncate">{mailbox}</span>
          <CopyIcon class="size-4 shrink-0" />
        </button>
      </Tooltip>
    </div>
    <div
      class="flex flex-col gap-2 border-b border-border-tertiary px-4 py-3 sm:flex-row sm:items-center sm:gap-3"
    >
      <span
        class="text-text-tertiary w-16 shrink-0 text-xs font-semibold uppercase"
      >
        {$t`From`}
      </span>
      <span class="text-text-primary truncate font-mono text-sm">
        {fromAddress}
      </span>
    </div>
    <div
      class="flex flex-col gap-2 border-b border-border-tertiary px-4 py-3 sm:flex-row sm:items-center sm:gap-3"
    >
      <span
        class="text-text-tertiary w-16 shrink-0 text-xs font-semibold uppercase"
      >
        {$t`Subject`}
      </span>
      <Tooltip
        label={$t`Copied to clipboard`}
        hidden={!copiedSubject}
        manual
        class="grow"
      >
        <button
          class="btn btn-secondary btn-sm w-full justify-between font-mono"
          onclick={copySubject}
          aria-label={$t`Copy subject`}
        >
          <span class="truncate">{nonce}</span>
          <CopyIcon class="size-4 shrink-0" />
        </button>
      </Tooltip>
    </div>
    <div
      class="flex flex-col gap-2 px-4 py-3 sm:flex-row sm:items-center sm:gap-3"
    >
      <span
        class="text-text-tertiary w-16 shrink-0 text-xs font-semibold uppercase"
      >
        {$t`Body`}
      </span>
      <span class="text-text-tertiary text-sm italic">
        {$t`(anything — leave it blank)`}
      </span>
    </div>
  </div>

  <a class="btn btn-primary btn-lg" href={mailtoHref}>
    <MailIcon class="size-5" />
    <span>{$t`Open in mail app`}</span>
  </a>

  <div
    class="text-text-tertiary flex flex-col items-center gap-1.5 text-sm"
    role="status"
  >
    <Loader2Icon class="text-fg-tertiary size-5 animate-spin" />
    <span>{$t`Waiting for your email to arrive…`}</span>
    {#if remainingSecs > 0}
      <span class="text-xs">
        <Trans>Expires in {remainingLabel}.</Trans>
      </span>
    {/if}
  </div>
</div>
