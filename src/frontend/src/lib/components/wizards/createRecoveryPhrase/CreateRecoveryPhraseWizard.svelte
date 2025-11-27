<script lang="ts">
  import Acknowledge from "$lib/components/wizards/createRecoveryPhrase/views/Acknowledge.svelte";
  import Write from "$lib/components/wizards/createRecoveryPhrase/views/Write.svelte";
  import VerifySelecting from "$lib/components/wizards/createRecoveryPhrase/views/VerifySelecting.svelte";
  import VerifyTyping from "$lib/components/wizards/createRecoveryPhrase/views/VerifyTyping.svelte";
  import { generateMnemonic } from "$lib/utils/recoveryPhrase";
  import Reset from "$lib/components/wizards/createRecoveryPhrase/views/Reset.svelte";
  import Retry from "$lib/components/wizards/createRecoveryPhrase/views/Retry.svelte";

  interface Props {
    action?: "create" | "verify";
    onCreate: (recoveryPhrase: string[]) => Promise<void>;
    onVerify: (recoveryPhrase: string[]) => Promise<boolean>;
    onCancel: () => void;
    onError: (error: unknown) => void;
    unverifiedRecoveryPhrase?: string[];
    hasExistingRecoveryPhrase?: boolean;
  }

  const {
    action = "create",
    onCreate,
    onVerify,
    onCancel,
    onError,
    unverifiedRecoveryPhrase,
    hasExistingRecoveryPhrase,
  }: Props = $props();

  let recoveryPhrase = $state<string[] | undefined>(
    action === "verify" ? unverifiedRecoveryPhrase : undefined,
  );
  let isWritten = $state(action === "verify");
  let isIncorrect = $state(false);
  let incorrectRecoveryPhrase = $state<string[]>();

  const createRecoveryPhrase = async () => {
    try {
      const generated = generateMnemonic();
      await onCreate(generated);
      recoveryPhrase = generated;
    } catch (error) {
      onError(error);
    }
  };
  const verifyRecoveryPhrase = async (entered: string[]) => {
    try {
      isIncorrect = !(await onVerify(entered));
      incorrectRecoveryPhrase = isIncorrect ? entered : undefined;
    } catch (error) {
      onError(error);
    }
  };
  const retryVerification = () => {
    isWritten = false;
    isIncorrect = false;
  };
</script>

{#if action === "create" && recoveryPhrase === undefined}
  {#if hasExistingRecoveryPhrase}
    <Reset onReset={createRecoveryPhrase} {onCancel} />
  {:else}
    <Acknowledge onAcknowledged={createRecoveryPhrase} />
  {/if}
{:else if !isWritten && recoveryPhrase !== undefined}
  <Write {recoveryPhrase} onWritten={() => (isWritten = true)} />
{:else if isIncorrect}
  <Retry
    onRetry={retryVerification}
    {onCancel}
    verificationMethod={recoveryPhrase !== undefined ? "selecting" : "typing"}
  />
{:else if recoveryPhrase !== undefined}
  <VerifySelecting {recoveryPhrase} onCompleted={verifyRecoveryPhrase} />
{:else}
  <VerifyTyping
    onCompleted={verifyRecoveryPhrase}
    recoveryPhrase={incorrectRecoveryPhrase}
  />
{/if}
