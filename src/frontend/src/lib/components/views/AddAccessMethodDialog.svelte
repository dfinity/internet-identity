<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import { handleError } from "../utils/error";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { StartAddPasskeyFlow } from "$lib/flows/startAddPasskeyFlow.svelte";
  import QrCreator from "qr-creator";
  import Input from "../ui/Input.svelte";
  import { isDarkMode } from "$lib/state/UI/isDarkMode";
  import { CROSS_DEVICE_PASSKEYS } from "$lib/state/featureFlags";

  const { onClose } = $props();

  const startAddPasskeyFlow = new StartAddPasskeyFlow(
    $authenticatedStore.identityNumber,
  );

  let inputRef = $state<HTMLInputElement>();
  let qrCodeRef = $state<HTMLDivElement>();

  let startAddingPasskeyDialog = $state(false);
  let url = $derived(
    `${window.location.origin}/flow/?user=${$authenticatedStore.identityNumber}`,
  );

  $effect(() => {
    if (qrCodeRef) {
      // Remove existing children when dark mode settings change
      qrCodeRef.innerHTML = "";
      QrCreator.render(
        {
          text: url,
          radius: 0.5,
          ecLevel: "H",
          fill: $isDarkMode ? "#fafafa" : "#1d1d1f",
          background: null,
          size: 200,
        },
        qrCodeRef,
      );
    }
  });

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
      <div class="mb-8 flex items-center justify-center">
        <div bind:this={qrCodeRef}></div>
      </div>
      <p class="text-text-primary text-center">{url}</p>
    {:else if startAddPasskeyFlow.view === "authorize"}
      <h1 class="text-text-primary mb-3 text-2xl font-medium">Authorize</h1>
      <Input
        bind:element={inputRef}
        bind:value={authorizationCode}
        onkeydown={(e) => {
          if (e.key === "Enter") handleAuthorize();
        }}
        inputmode="text"
        placeholder="Authorization code"
        hint="Enter the authorization code displayed on the new client."
        type="text"
        size="md"
        autocomplete="off"
        autocorrect="off"
        spellcheck="false"
        error={authorizationCode.length > 8
          ? "Maximum length is 8 characters."
          : undefined}
        aria-label="Identity name"
        class="mb-3"
      />
      <Button variant="primary" onclick={handleAuthorize}>Authorize</Button>
    {:else if startAddPasskeyFlow.view === "success"}
      <h1 class="text-text-primary mb-3 text-2xl font-medium">
        On this side, you're finished.
      </h1>
      <p class="text-text-tertiary mb-3 font-medium">
        Please continue on the new device. You can close this window.
      </p>
      <Button onclick={onClose}>Close</Button>
    {/if}
  {:else}
    <h1 class="text-text-primary mb-3 text-2xl font-medium">Add credential</h1>
    <p class="text-text-tertiary mb-8 font-medium">
      Please choose the type of credential you would like to add.
    </p>
    <div class="flex w-full flex-col gap-3">
      {#if $CROSS_DEVICE_PASSKEYS}
        <Button
          variant="primary"
          onclick={() => {
            chooseAddPasskey();
          }}
        >
          Add Passkey on other Device
        </Button>
      {/if}
      <!-- TODO: if/when we add more credentials and OpenID providers, we'll need to add more buttons here -->
      {#if identityInfo.openIdCredentials.length === 0}
        <Button
          variant="primary"
          onclick={() => {
            onClose();
            handleAddCredential();
          }}
        >
          <GoogleIcon /> Link Google Account
        </Button>
      {/if}
      <Button variant="tertiary" onclick={onClose}>Cancel</Button>
    </div>
  {/if}
</Dialog>
