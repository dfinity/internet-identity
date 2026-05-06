<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { MailXIcon } from "@lucide/svelte";

  interface Props {
    /** Human-readable summary of what went wrong. The wizard maps
        canister error variants to friendly strings before passing
        them in. */
    reason: string;
    onRetry: () => void;
    onCancel: () => void;
  }

  const { reason, onRetry, onCancel }: Props = $props();
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col items-center gap-3">
    <MailXIcon class="text-fg-error-primary size-10" />
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Verification failed`}
    </h1>
    <p class="text-text-tertiary text-center text-sm">
      <Trans>{reason}</Trans>
    </p>
  </header>
  <div class="flex flex-row justify-end gap-2">
    <Button onclick={onCancel} variant="secondary" type="button">
      {$t`Cancel`}
    </Button>
    <Button onclick={onRetry} type="button">{$t`Try again`}</Button>
  </div>
</div>
