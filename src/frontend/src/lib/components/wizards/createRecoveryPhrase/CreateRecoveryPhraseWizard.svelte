<script lang="ts">
  import Acknowledge from "$lib/components/wizards/createRecoveryPhrase/views/Acknowledge.svelte";
  import Write from "$lib/components/wizards/createRecoveryPhrase/views/Write.svelte";
  import Verify from "$lib/components/wizards/createRecoveryPhrase/views/Verify.svelte";
  import { generateMnemonic } from "$lib/utils/recoveryPhrase";
  import Reset from "$lib/components/wizards/createRecoveryPhrase/views/Reset.svelte";

  interface Props {
    onCreate: (recoveryPhrase: string[]) => Promise<void>;
    onVerified: () => Promise<void>;
    onCancel: () => void;
    unverifiedRecoveryPhrase?: string[];
    hasExistingRecoveryPhrase?: boolean;
  }

  const {
    onCreate,
    onVerified,
    onCancel,
    unverifiedRecoveryPhrase,
    hasExistingRecoveryPhrase,
  }: Props = $props();

  let recoveryPhrase = $state(unverifiedRecoveryPhrase);
  let isWritten = $state(unverifiedRecoveryPhrase !== undefined);

  const createRecoveryPhrase = async () => {
    const generated = generateMnemonic();
    await onCreate(generated);
    recoveryPhrase = generated;
  };
</script>

{#if recoveryPhrase === undefined}
  {#if hasExistingRecoveryPhrase}
    <Reset onReset={createRecoveryPhrase} {onCancel} />
  {:else}
    <Acknowledge onAcknowledged={createRecoveryPhrase} />
  {/if}
{:else if !isWritten}
  <Write {recoveryPhrase} onWritten={() => (isWritten = true)} />
{:else}
  <Verify {recoveryPhrase} {onVerified} />
{/if}
