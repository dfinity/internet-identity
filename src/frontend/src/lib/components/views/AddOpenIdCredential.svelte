<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import { handleError } from "../utils/error";

  const { onClose } = $props();

  const handleAddCredential = async () => {
    try {
      await identityInfo.addGoogle();
    } catch (error) {
      handleError(error);
    }
  };
</script>

<Dialog {onClose}>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">Add credential</h1>
  <p class="text-text-tertiary mb-8 font-medium">
    Please choose the type of credential you would like to add.
  </p>
  <div class="flex w-full flex-col gap-3">
    <!-- TODO: if/when we add more credentials and OpenID providers, we'll need to add more buttons here -->
    <Button
      variant="primary"
      onclick={() => {
        onClose();
        handleAddCredential();
      }}
    >
      <GoogleIcon /> Link Google Account
    </Button>
    <Button variant="tertiary" onclick={onClose}>Cancel</Button>
  </div>
</Dialog>
