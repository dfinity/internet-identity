<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import Button from "$lib/components/ui/Button.svelte";

  interface Props {
    recoveryPhrase: string[];
    onVerified: () => Promise<void>;
  }

  const { onVerified }: Props = $props();
</script>

<h2>{$t`Verify your recovery phrase`}</h2>
<!-- TODO: implement word verification -->
<button onclick={onVerified}>Mark as verified</button>
