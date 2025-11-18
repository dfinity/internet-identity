<script lang="ts">
  import { t } from "$lib/stores/locale.store";

  interface Props {
    recoveryPhrase: string[];
    onVerified: () => Promise<void>;
  }

  const { onVerified }: Props = $props();
</script>

<h2>{$t`Verify your recovery phrase`}</h2>
<!-- TODO: implement word verification -->
<button onclick={onVerified}>Mark as verified</button>
