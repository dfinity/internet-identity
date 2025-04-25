<script lang="ts">
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { nonNullish } from "@dfinity/utils";
  import { onMount } from "svelte";

  type Props = {
    connectOrCreatePasskey: () => void;
    authenticateWithGoogle: () => void;
  };

  const { connectOrCreatePasskey, authenticateWithGoogle }: Props = $props();

  onMount(() => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.SelectMethodScreen);
  });

  const supportsPasskeys = nonNullish(window.PublicKeyCredential);

  const handleContinueWithGoogle = () => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueWithGoogle);
    authenticateWithGoogle();
  };
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
  <button onclick={handleContinueWithGoogle} class="btn preset-outlined py-2"
    >Continue with Google</button
  >
</div>
