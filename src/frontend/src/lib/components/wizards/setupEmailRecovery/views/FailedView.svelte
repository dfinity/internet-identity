<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { MailXIcon } from "@lucide/svelte";

  interface Props {
    /** Human-readable summary of what went wrong. The wizard maps
     *  canister error variants to friendly strings before passing
     *  them in. */
    reason: string;
    onRetry: () => void;
  }

  const { reason, onRetry }: Props = $props();
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col items-center gap-3 text-center">
    <MailXIcon class="text-fg-error-primary size-10" />
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Verification failed`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">{reason}</p>
  </header>
  <button class="btn btn-primary btn-lg" type="button" onclick={onRetry}>
    {$t`Try again`}
  </button>
</div>
