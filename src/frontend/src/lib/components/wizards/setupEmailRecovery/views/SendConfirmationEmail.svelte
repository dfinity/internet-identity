<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { CopyIcon, ExternalLinkIcon, ShieldCheckIcon } from "@lucide/svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
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
    /** Which DNS-verification path the canister will run when the
     *  email arrives — drives the trust-story tooltip on the From row.
     *  "dnssec" when the FE submitted a signed bundle; "doh" when the
     *  canister will fall back to its multi-provider DoH quorum. */
    path: "dnssec" | "doh";
  }

  const { nonce, mailbox, fromAddress, path }: Props = $props();

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

  const verificationDescription = $derived(
    path === "dnssec"
      ? $t`Your provider's DKIM key is verified end-to-end via DNSSEC. No trusted intermediaries.`
      : $t`Your provider's DKIM key is verified by quorum consensus across independent DNS resolvers.`,
  );
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col gap-2">
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Verify your email`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">
      <Trans>Send the email below to confirm.</Trans>
    </p>
  </header>

  <!-- Pre-filled email card. Each row places its label above the value
       so the dialog stays comfortably narrow. To and Subject rows are
       whole-row click targets that copy their value to the clipboard.
       From and Body rows are plain text. -->
  <div
    class="bg-bg-primary border-border-secondary flex flex-col overflow-hidden rounded-xl border not-dark:shadow-sm"
  >
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

    <div class="border-border-tertiary flex flex-col gap-1 border-b px-4 py-3">
      <span
        class="text-text-tertiary text-xs font-semibold tracking-wide uppercase"
      >
        {$t`From`}
      </span>
      <span class="flex items-center gap-2">
        <span class="text-text-primary grow truncate font-mono text-sm">
          {fromAddress}
        </span>
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
            class="btn btn-tertiary btn-icon text-fg-success-primary !cursor-default !rounded-full"
            aria-label={$t`Cryptographically authentic`}
          >
            <ShieldCheckIcon class="size-5" aria-hidden="true" />
          </button>
        </Tooltip>
      </span>
    </div>

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
  </div>

  <a class="btn btn-primary btn-lg" href={mailtoHref}>
    <ExternalLinkIcon class="size-5" />
    <span>{$t`Open in mail app`}</span>
  </a>
</div>
