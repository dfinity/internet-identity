<script lang="ts">
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { nonNullish } from "@dfinity/utils";

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
  Confirm your sign-in
</h1>
<p class="text-md text-text-tertiary mb-8 font-medium text-balance">
  {#if nonNullish(name)}
    You're signing in as <b class="text-text-primary">{name}</b>.
  {:else}
    You're about to sign in.
  {/if}
  <br /><br />
  To continue, create a passkey to secure your identity and simplify future sign-ins.
</p>
<Button onclick={handleCreatePasskey} size="xl" disabled={isCreatingPasskey}>
  {#if isCreatingPasskey}
    <ProgressRing />
    <span>Creating passkey...</span>
  {:else}
    <span>Create passkey</span>
  {/if}
</Button>
