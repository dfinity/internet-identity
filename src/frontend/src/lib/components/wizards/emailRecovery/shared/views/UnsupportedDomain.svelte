<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { HelpCircleIcon, ChevronDownIcon } from "@lucide/svelte";

  interface Props {
    /** Domain part of the address the user typed, e.g. `acme.example`. */
    domain: string;
    /** Back to the address-entry view. */
    onRetry: () => void;
  }

  const { domain, onRetry }: Props = $props();
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col items-center gap-3 text-center">
    <HelpCircleIcon class="text-fg-tertiary size-10" />
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Can't use this email`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">
      <Trans>
        We can't verify mail from
        <strong class="break-all">{domain}</strong>
        yet. Try a different address. Gmail, iCloud, Outlook, Fastmail, and Proton
        all work.
      </Trans>
    </p>
  </header>

  <details
    class="bg-bg-primary border-border-secondary group rounded-xl border px-4 py-3 not-dark:shadow-sm"
  >
    <summary
      class="text-text-primary flex cursor-pointer list-none items-center justify-between text-sm font-medium select-none"
    >
      <span>{$t`Why isn't this supported?`}</span>
      <ChevronDownIcon
        class="text-fg-tertiary size-4 transition-transform group-open:rotate-180"
      />
    </summary>
    <div
      class="text-text-tertiary mt-3 flex flex-col gap-3 text-sm font-medium"
    >
      <p>
        <Trans>
          We verify an email by checking the signature its provider adds, using
          a key published in the domain's DNS.
          <strong class="break-all">{domain}</strong> isn't a domain we can look that
          key up for, so emails from this address can't be verified.
        </Trans>
      </p>
    </div>
  </details>

  <button class="btn btn-primary btn-lg" type="button" onclick={onRetry}>
    {$t`Try a different address`}
  </button>
</div>
