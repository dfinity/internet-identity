<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import Button from "$lib/components/ui/Button.svelte";

  interface Props {
    onAcknowledged: () => Promise<void>;
  }

  const { onAcknowledged }: Props = $props();

  let isAcknowledged = $state(false);
</script>

<h2>{$t`Secure your identity`}</h2>
<Checkbox
  bind:checked={isAcknowledged}
  label={$t`I Acknowledge`}
  hint={$t`My recovery phrase isn’t stored by Internet Identity. I will keep it offline in a safe place.`}
/>
<Button onclick={onAcknowledged} disabled={!isAcknowledged}>
  {$t`Continue`}
</Button>
