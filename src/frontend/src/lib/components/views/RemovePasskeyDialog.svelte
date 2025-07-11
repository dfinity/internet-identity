<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";

  interface Props {
    onRemove: () => void;
    onClose: () => void;
    isCurrentAccessMethod?: boolean;
  }

  const { onRemove, onClose, isCurrentAccessMethod = false }: Props = $props();
</script>

<Dialog {onClose}>
  <FeaturedIcon variant="warning" size="lg" class="mb-3">
    <TriangleAlertIcon size="1.5rem" />
  </FeaturedIcon>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">Are you sure?</h1>
  <p class="text-text-tertiary mb-8 font-medium">
    Removing this passkey means you won't be able to use it to sign in anymore.
    You can always add a new one later.
    {#if isCurrentAccessMethod}
      <br /><br />
      As you are currently signed in with this passkey, you will be signed out.
    {/if}
  </p>
  <Button onclick={onRemove} variant="primary" danger class="mb-3">
    Remove passkey
  </Button>
  <Button onclick={onClose} variant="tertiary">Cancel</Button>
</Dialog>
