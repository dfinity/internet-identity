<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import RecoveryPhraseInput from "$lib/components/views/RecoveryPhraseInput.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { LockKeyholeIcon } from "@lucide/svelte";

  interface Props {
    onCompleted: (recoveryPhrase: string[]) => Promise<void>;
    recoveryPhrase?: string[];
  }

  const { onCompleted, recoveryPhrase }: Props = $props();

  let isCheckingPhrase = $state(false);

  const handleSubmit = async (recoveryPhrase: string[]) => {
    try {
      isCheckingPhrase = true;
      await onCompleted(recoveryPhrase);
    } finally {
      isCheckingPhrase = false;
    }
  };
</script>

<div class={["mb-4", "[@media(max-height:640px)]:hidden"]}>
  <FeaturedIcon size="md">
    <LockKeyholeIcon class="size-5" />
  </FeaturedIcon>
</div>
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {#if isCheckingPhrase}
    {$t`Checking recovery phrase`}
  {:else}
    {$t`Unlock to continue`}
  {/if}
</h2>
<p class="text-text-tertiary mb-5 text-base font-medium">
  {#if isCheckingPhrase}
    <Trans>This may take a few seconds</Trans>
  {:else}
    <Trans>Enter each word in the correct order:</Trans>
  {/if}
</p>
<RecoveryPhraseInput value={recoveryPhrase} onSubmit={handleSubmit} />
