<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";

  interface Props {
    onRemove: () => void;
    onClose: () => void;
    isCurrentAccessMethod?: boolean;
  }

  const { onRemove, onClose, isCurrentAccessMethod }: Props = $props();
</script>

<Dialog {onClose}>
  <FeaturedIcon class="mb-3" variant="warning">
    <TriangleAlertIcon />
  </FeaturedIcon>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">Are you sure?</h1>
  <p class="text-text-tertiary mb-8 font-medium">
    You're about to unlink your Google Account. If you proceed, you will no
    longer be able to sign-in to your identity or dapps using your Google
    Account.
    {#if isCurrentAccessMethod}
      <br /><br />As you are currently signed in with this Account, you will be
      signed out.
    {/if}
  </p>

  <div class="flex w-full flex-col gap-3">
    <Button onclick={onRemove} variant="primary" danger>
      Unlink Google Account
    </Button>
    <Button variant="tertiary" onclick={onClose}>Keep linked</Button>
  </div>
</Dialog>
