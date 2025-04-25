<script lang="ts">
  import { fly } from "svelte/transition";
  import { type State } from "../state.js";
  import { onMount } from "svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";

  type Props = Extract<State, { state: "connectOrCreatePasskey" }>;

  const { connect, create }: Props = $props();

  onMount(() => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.ContinueWithPasskeyScreen,
    );
  });
</script>

<div
  class="mb-8 flex flex-col gap-3 opacity-80"
  in:fly={{ duration: 200, x: -10 }}
>
  <p>
    Passkeys offer a secure, password-free method for logging into your
    accounts, eliminating the need to remember complex passwords.
  </p>
  <p>
    A unique cryptographic key is stored on your device, enabling a faster, more
    secure, and reliable way to authenticate your identity.
  </p>
</div>
<div class="mt-auto flex flex-col items-stretch gap-4">
  <button onclick={create} class="btn preset-filled py-2"
    >Set up a new Passkey</button
  >
  <button onclick={connect} class="btn preset-outlined py-2"
    >Use an existing Passkey</button
  >
</div>
