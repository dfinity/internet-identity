<script lang="ts">
  import BottomCard from "./BottomCard.svelte";
  import Button from "./Button.svelte";

  const {
    close,
    handleConnectPasskey,
    handleGotoCreatePasskey,
  }: {
    close: () => void;
    handleConnectPasskey: () => void;
    handleGotoCreatePasskey: () => void;
  } = $props();
</script>

<BottomCard {close}>
  <div class="flex flex-col items-center gap-2 p-4">
    <h3 class="h3 mb-2 w-full max-w-96">[Activate a passkey]</h3>
    <Button
      onclick={handleConnectPasskey}
      class="w-full max-w-96"
      variant="primary">Connect my Passkey</Button
    >
    <Button
      onclick={handleGotoCreatePasskey}
      class="w-full max-w-96"
      variant="secondary">Don't have a passkey? Create one</Button
    >
  </div>
</BottomCard>
