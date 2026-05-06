<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { MailOpenIcon, CopyIcon, CheckIcon } from "@lucide/svelte";
  import { onMount, onDestroy } from "svelte";

  interface Props {
    /** The canister-issued nonce (e.g. `II-Recovery-1a2b3c4d…`). */
    nonce: string;
    /** Recipient mailbox the user emails to: `register@id.ai`. */
    mailbox: string;
    /** Address the user typed at step 1 — shown back so they don't
        send the email from the wrong account. */
    fromAddress: string;
    /** Unix-ns timestamp at which the canister-side challenge expires. */
    expiresAt: bigint;
    /** Cancel the wizard. */
    onCancel: () => void;
  }

  const { nonce, mailbox, fromAddress, expiresAt, onCancel }: Props = $props();

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

  let copied = $state(false);
  let copyTimer: ReturnType<typeof setTimeout> | undefined;
  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(nonce);
      copied = true;
      if (copyTimer) clearTimeout(copyTimer);
      copyTimer = setTimeout(() => {
        copied = false;
      }, 2_000);
    } catch {
      // Clipboard API may be unavailable in restricted contexts —
      // fail silently. The user can select+copy manually.
    }
  };
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col items-center gap-3">
    <MailOpenIcon class="text-fg-brand-primary size-10" />
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Send the magic email`}
    </h1>
    <p class="text-text-tertiary text-center text-sm">
      <Trans>
        Send a fresh email from
        <strong>{fromAddress}</strong>
        to <strong>{mailbox}</strong> with the token below as the subject.
        We're watching for it — this page will refresh automatically.
      </Trans>
    </p>
  </header>

  <div class="flex flex-col gap-2">
    <span class="text-text-secondary text-xs font-medium uppercase">
      {$t`Subject`}
    </span>
    <div
      class="bg-bg-secondary border-border-secondary flex items-center gap-3 rounded-lg border px-4 py-3 font-mono text-sm"
    >
      <span class="grow break-all">{nonce}</span>
      <button
        type="button"
        onclick={handleCopy}
        class="text-fg-brand-primary hover:bg-bg-primary shrink-0 rounded p-1"
        aria-label={$t`Copy token`}
      >
        {#if copied}
          <CheckIcon class="size-4" />
        {:else}
          <CopyIcon class="size-4" />
        {/if}
      </button>
    </div>
  </div>

  <div class="bg-bg-warning-secondary text-text-primary rounded-lg p-4 text-sm">
    <Trans>
      Only continue if you started this on this page, just now. Sending the
      magic email will activate this address as a recovery method.
    </Trans>
  </div>

  <p class="text-text-tertiary text-center text-xs">
    <Trans>This token expires in {remainingLabel}.</Trans>
  </p>

  <div class="flex flex-row justify-end">
    <Button onclick={onCancel} variant="secondary" type="button">
      {$t`Cancel`}
    </Button>
  </div>
</div>
