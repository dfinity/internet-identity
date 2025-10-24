<script lang="ts">
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { nonNullish } from "@dfinity/utils";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    name?: string;
    createPasskey: () => Promise<void>;
  }

  const { name, createPasskey }: Props = $props();

  let isCreatingPasskey = $state(false);

  const handleCreatePasskey = async () => {
    isCreatingPasskey = true;
    try {
      await createPasskey();
    } finally {
      isCreatingPasskey = false;
    }
  };
</script>

<PasskeyIllustration class="text-text-primary mt-4 mb-8 h-32" />
<h1 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`Confirm your sign-in`}
</h1>
<p class="text-text-tertiary mb-4 text-base font-medium text-balance">
  {#if nonNullish(name)}
    <Trans>
      You're signing in as <b class="text-text-primary">{name}</b>.
    </Trans>
  {:else}
    <Trans>You're about to sign in.</Trans>
  {/if}
</p>
<p class="text-text-tertiary mb-8 text-base font-medium text-balance">
  <Trans>
    To continue, create a passkey to secure your identity and simplify future
    sign-ins.
  </Trans>
</p>
<Button onclick={handleCreatePasskey} size="xl" disabled={isCreatingPasskey}>
  {#if isCreatingPasskey}
    <ProgressRing />
    <span>{$t`Creating passkey...`}</span>
  {:else}
    <span>{$t`Create passkey`}</span>
  {/if}
</Button>
