<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Button from "$lib/components/ui/Button.svelte";

  interface Props {
    recoveryPhrase: string[];
    onWritten: () => void;
  }

  const { recoveryPhrase, onWritten }: Props = $props();
</script>

<h2>{$t`Save your recovery phrase`}</h2>
<p>
  <Trans>
    Write down (in order), store safely. Don't share. Lose it, lose access. Make
    sure to verify your recovery phrase after you have written it down.
  </Trans>
</p>
<div class="grid grid-cols-3 gap-1">
  {#each recoveryPhrase as word}
    <div>{word}</div>
  {/each}
</div>
<Button onclick={onWritten}>{$t`I have written it down`}</Button>
