<script lang="ts">
  import Acknowledge from "$lib/components/wizards/createRecoveryPhrase/views/Acknowledge.svelte";
  import Write from "$lib/components/wizards/createRecoveryPhrase/views/Write.svelte";
  import Verify from "$lib/components/wizards/createRecoveryPhrase/views/Verify.svelte";
  import { generateMnemonic } from "$lib/utils/recoveryPhrase";
  import Reset from "$lib/components/wizards/createRecoveryPhrase/views/Reset.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Retry from "$lib/components/wizards/createRecoveryPhrase/views/Retry.svelte";

  interface Props {
    onCreate: (recoveryPhrase: string[]) => Promise<void>;
    onVerified: () => void;
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
  let isIncorrect = $state(false);

  const createRecoveryPhrase = async () => {
    const generated = generateMnemonic();
    await onCreate(generated);
    recoveryPhrase = generated;
  };
  const verifyRecoveryPhrase = async (entered: string[]) => {
    // Artificial delay to improve UX, instant feedback would be strange
    // after the user spent some time on selecting words one after another.
    await waitFor(2000);
    if (recoveryPhrase?.join(" ") !== entered.join(" ")) {
      isIncorrect = true;
      return;
    }
    onVerified();
  };
  const retryVerification = () => {
    isWritten = false;
    isIncorrect = false;
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
{:else if isIncorrect}
  <Retry onRetry={retryVerification} {onCancel} />
{:else}
  <Verify {recoveryPhrase} onCompleted={verifyRecoveryPhrase} />
{/if}
