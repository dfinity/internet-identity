<script lang="ts">
  import Acknowledge from "$lib/components/wizards/createRecoveryPhrase/views/Acknowledge.svelte";
  import Write from "$lib/components/wizards/createRecoveryPhrase/views/Write.svelte";
  import Verify from "$lib/components/wizards/createRecoveryPhrase/views/Verify.svelte";
  import { generateMnemonic } from "$lib/utils/recoveryPhrase";

  interface Props {
    onAcknowledged: (recoveryPhrase: string[]) => Promise<void>;
    onVerified: () => Promise<void>;
    unverifiedRecoveryPhrase?: string[];
  }

  const { onAcknowledged, onVerified, unverifiedRecoveryPhrase }: Props =
    $props();

  let recoveryPhrase = $state(unverifiedRecoveryPhrase);
  let isWritten = $state(unverifiedRecoveryPhrase !== undefined);

  const handleAcknowledged = async () => {
    const generated = generateMnemonic();
    await onAcknowledged(generated);
    recoveryPhrase = generated;
  };
</script>

{#if recoveryPhrase === undefined}
  <Acknowledge onAcknowledged={handleAcknowledged} />
{:else if !isWritten}
  <Write {recoveryPhrase} onWritten={() => (isWritten = true)} />
{:else}
  <Verify {recoveryPhrase} {onVerified} />
{/if}
