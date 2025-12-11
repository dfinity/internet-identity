<script lang="ts">
  import RecoveryPhraseInput from "$lib/components/views/RecoveryPhraseInput.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    recoveryPhrase?: string[];
    onSubmit: (recoveryPhrase: string[]) => Promise<void>;
  }

  const { recoveryPhrase, onSubmit }: Props = $props();
</script>

<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`Enter recovery phrase`}
</h2>
<p class="text-text-tertiary mb-6 text-base font-medium">
  <Trans>Type each word in the correct order:</Trans>
</p>
<RecoveryPhraseInput value={recoveryPhrase} {onSubmit} />
