<script lang="ts">
  import { nonNullish } from "@dfinity/utils";

  type Props = {
    connectOrCreatePasskey: () => void;
    continueWithGoogle: () => void;
  };

  const { connectOrCreatePasskey, continueWithGoogle }: Props = $props();

  const supportsPasskeys = nonNullish(window.PublicKeyCredential);
</script>

<div class="flex flex-col items-stretch gap-4">
  {#if !supportsPasskeys}
    <div class="card preset-filled-surface-100-900 p-4">
      <p class="font-semibold">Passkeys are unavailable on this browser</p>
      <p class="text-sm">Please choose another sign-in method</p>
    </div>
  {/if}
  <button
    onclick={connectOrCreatePasskey}
    class="btn preset-filled py-2"
    disabled={!supportsPasskeys}>Continue with Passkey</button
  >
  <button onclick={continueWithGoogle} class="btn preset-outlined py-2"
    >Continue with Google</button
  >
</div>
