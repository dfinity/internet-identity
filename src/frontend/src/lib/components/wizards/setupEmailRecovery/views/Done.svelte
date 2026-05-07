<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { MailCheckIcon } from "@lucide/svelte";
  import Steps from "$lib/components/wizards/createRecoveryPhrase/components/Steps.svelte";

  interface Props {
    address: string;
    onDone: () => void;
  }

  const { address, onDone }: Props = $props();
</script>

<div class="flex flex-col gap-6">
  <div class="my-2"><Steps total={3} current={3} /></div>
  <header class="flex flex-col items-center gap-3 text-center">
    <MailCheckIcon class="text-fg-success-primary size-10" />
    <h1 class="text-text-primary text-2xl font-medium">{$t`All set`}</h1>
    <p class="text-text-tertiary text-base font-medium">
      <Trans>
        <strong class="break-all">{address}</strong>
        is now a recovery method.
      </Trans>
    </p>
  </header>
  <button class="btn btn-primary btn-lg" onclick={onDone}>{$t`Done`}</button>
</div>
