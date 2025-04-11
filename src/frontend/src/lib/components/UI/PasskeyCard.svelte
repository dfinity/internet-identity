<script lang="ts">
  import Button from "./Button.svelte";

  const {
    handleConnectPasskey,
    handleGotoCreateIdentity,
    close,
    ...rest
  }: {
    handleConnectPasskey: () => void;
    handleGotoCreateIdentity: () => void;
    [key: string]: any;
    close: () => void;
  } = $props();
</script>

<div class="flex flex-col items-center gap-2 p-4" {...rest}>
  <h3 class="h3 mb-2 w-full max-w-96">[Activate a passkey]</h3>
  <Button
    onclick={handleConnectPasskey}
    class="w-full max-w-96"
    variant="primary">Connect my Passkey</Button
  >
  <Button
    onclick={handleGotoCreateIdentity}
    class="w-full max-w-96"
    variant="secondary">Don't have a passkey? Create one</Button
  >
  <Button class="w-full max-w-96" onclick={close} variant="secondary"
    >Back</Button
  >
</div>
