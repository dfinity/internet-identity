<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";
  import { fly } from "svelte/transition";
  import { type State } from "../state.js";

  type Props = Extract<State, { state: "connectOrCreatePasskey" }>;

  const { connect, create }: Props = $props();
</script>

<div class="mb-8 flex flex-col gap-4" in:fly={{ duration: 200, x: -10 }}>
  <p>
    Passkeys offer a secure, password-free method for logging into your
    accounts, eliminating the need to remember complex passwords.
  </p>
  <p>
    A unique cryptographic key is stored on your device, enabling a faster, more
    secure, and reliabe way to authenticate your identity.
  </p>
</div>
<div class="mt-auto flex flex-col items-stretch gap-4">
  <Button onclick={create} variant="secondary">Create new passkey</Button>
  <Button onclick={connect} variant="secondary">Already have a passkey?</Button>
</div>
