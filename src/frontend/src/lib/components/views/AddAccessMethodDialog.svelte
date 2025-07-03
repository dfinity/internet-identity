<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import { handleError } from "../utils/error";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { StartAddPasskeyFlow } from "$lib/flows/startAddPasskeyFlow.svelte";

  const { onClose } = $props();

  const startAddPasskeyFlow = new StartAddPasskeyFlow(
    $authenticatedStore.identityNumber,
  );

  let startAddingPasskeyDialog = $state(false);
  let url = $derived(
    `${window.location.origin}/flow/?user=${$authenticatedStore.identityNumber}`,
  );

  const handleAddCredential = async () => {
    try {
      await identityInfo.addGoogle();
    } catch (error) {
      handleError(error);
    }
  };

  const chooseAddPasskey = async () => {
    startAddingPasskeyDialog = true;
    await startAddPasskeyFlow.startAddPasskeyFlow();
  };

  let authorizationCode = $state("");

  const handleAuthorize = async () => {
    startAddingPasskeyDialog = false;
    await startAddPasskeyFlow.verifyDevice(authorizationCode);
  };
</script>

<Dialog {onClose}>
  {#if startAddingPasskeyDialog}
    {#if startAddPasskeyFlow.view === "show-link"}
      <h1 class="text-text-primary mb-3 text-2xl font-medium">
        Continue on another device
      </h1>
      <p class="text-text-tertiary mb-8 font-medium">
        Scan the following QR code below on your new device or enter the URL
        manually
      </p>
      <a class="text-text-primary" href={url}>{url}</a>
    {:else if startAddPasskeyFlow.view === "authorize"}
      <h1 class="text-text-primary mb-3 text-2xl font-medium">Authorize</h1>
      <input type="text" bind:value={authorizationCode} />
      <Button variant="primary" onclick={handleAuthorize}>Authorize</Button>
    {:else if startAddPasskeyFlow.view === "success"}
      <h1 class="text-text-primary mb-3 text-2xl font-medium">Device added</h1>
      <p class="text-text-tertiary mb-8 font-medium">
        Your device has been added successfully.
      </p>
    {/if}
  {:else}
    <h1 class="text-text-primary mb-3 text-2xl font-medium">Add credential</h1>
    <p class="text-text-tertiary mb-8 font-medium">
      Please choose the type of credential you would like to add.
    </p>
    <div class="flex w-full flex-col gap-3">
      <Button
        variant="primary"
        onclick={() => {
          chooseAddPasskey();
        }}
      >
        Add Passkey
      </Button>
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
  {/if}
</Dialog>
