<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import Button from "$lib/components/ui/Button.svelte";

  interface Props {
    onReset: () => Promise<void>;
    onCancel: () => void;
  }

  const { onReset, onCancel }: Props = $props();
</script>

<h2>{$t`Reset your recovery phrase?`}</h2>
<Button onclick={onReset}>
  {$t`Reset`}
</Button>
<Button onclick={onCancel} variant="tertiary">
  {$t`Cancel`}
</Button>
