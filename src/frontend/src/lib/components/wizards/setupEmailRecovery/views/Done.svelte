<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { MailCheckIcon } from "@lucide/svelte";

  interface Props {
    address: string;
    onDone: () => void;
  }

  const { address, onDone }: Props = $props();
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col items-center gap-3">
    <MailCheckIcon class="text-fg-success-primary size-10" />
    <h1 class="text-text-primary text-2xl font-medium">{$t`All set`}</h1>
    <p class="text-text-tertiary text-center text-sm">
      <Trans>
        <strong class="break-all">{address}</strong>
        is now a recovery method for this Internet Identity. You can use it to
        sign in if you ever lose access to your other authentication methods.
      </Trans>
    </p>
  </header>
  <div class="flex flex-row justify-end">
    <Button onclick={onDone}>{$t`Done`}</Button>
  </div>
</div>
