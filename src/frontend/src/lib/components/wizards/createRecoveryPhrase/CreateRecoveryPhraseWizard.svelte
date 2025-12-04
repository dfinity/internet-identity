<script lang="ts">
  import Acknowledge from "$lib/components/wizards/createRecoveryPhrase/views/Acknowledge.svelte";
  import Write from "$lib/components/wizards/createRecoveryPhrase/views/Write.svelte";
  import VerifySelecting from "$lib/components/wizards/createRecoveryPhrase/views/VerifySelecting.svelte";
  import VerifyTyping from "$lib/components/wizards/createRecoveryPhrase/views/VerifyTyping.svelte";
  import { generateMnemonic } from "$lib/utils/recoveryPhrase";
  import Reset from "$lib/components/wizards/createRecoveryPhrase/views/Reset.svelte";
  import Retry from "$lib/components/wizards/createRecoveryPhrase/views/Retry.svelte";
  import Unlock from "$lib/components/wizards/createRecoveryPhrase/views/Unlock.svelte";

  interface Props {
    action?: "create" | "verify";
    onCreate: (recoveryPhrase: string[]) => Promise<void>;
    onVerify: (recoveryPhrase: string[]) => Promise<boolean>;
    onUnlock: (recoveryPhrase: string[]) => Promise<boolean>;
    onCancel: () => void;
    onError: (error: unknown) => void;
    unverifiedRecoveryPhrase?: string[];
    existingRecoveryPhraseType?: "unprotected" | "protected";
  }

  const {
    action = "create",
    onCreate,
    onVerify,
    onUnlock,
    onCancel,
    onError,
    unverifiedRecoveryPhrase,
    existingRecoveryPhraseType,
  }: Props = $props();

  let recoveryPhrase = $state<string[] | undefined>(
    action === "verify" ? unverifiedRecoveryPhrase : undefined,
  );
  let isWritten = $state(action === "verify");
  let isIncorrect = $state(false);
  let isLocked = $state(existingRecoveryPhraseType === "protected");
  let incorrectRecoveryPhrase = $state<string[]>();

  const handleCreate = async () => {
    try {
      const generated = generateMnemonic();
      await onCreate(generated);
      recoveryPhrase = generated;
    } catch (error) {
      onError(error);
    }
  };
  const handleVerify = async (entered: string[]) => {
    try {
      isIncorrect = !(await onVerify(entered));
      incorrectRecoveryPhrase = isIncorrect ? entered : undefined;
    } catch (error) {
      onError(error);
    }
  };
  const handleRetry = () => {
    isWritten = false;
    isIncorrect = false;
  };
  const handleUnlock = async (entered: string[]) => {
    try {
      isIncorrect = !(await onUnlock(entered));
      incorrectRecoveryPhrase = isIncorrect ? entered : undefined;
      isLocked = isIncorrect;
    } catch (error) {
      onError(error);
    }
  };
</script>

{#if action === "create" && recoveryPhrase === undefined}
  {#if existingRecoveryPhraseType !== undefined}
    {#if isIncorrect}
      <Retry onRetry={handleRetry} {onCancel} inputMethod="typing" />
    {:else if isLocked}
      <Unlock
        recoveryPhrase={incorrectRecoveryPhrase}
        onCompleted={handleUnlock}
      />
    {:else}
      <Reset onReset={handleCreate} {onCancel} />
    {/if}
  {:else}
    <Acknowledge onAcknowledged={handleCreate} />
  {/if}
{:else if !isWritten && recoveryPhrase !== undefined}
  <Write {recoveryPhrase} onWritten={() => (isWritten = true)} />
{:else if isIncorrect}
  <Retry
    onRetry={handleRetry}
    {onCancel}
    inputMethod={recoveryPhrase !== undefined ? "selecting" : "typing"}
  />
{:else if recoveryPhrase !== undefined}
  <VerifySelecting {recoveryPhrase} onCompleted={handleVerify} />
{:else}
  <VerifyTyping
    onCompleted={handleVerify}
    recoveryPhrase={incorrectRecoveryPhrase}
  />
{/if}
