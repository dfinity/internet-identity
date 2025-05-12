<script lang="ts">
  import { fly } from "svelte/transition";
  import { type State } from "../state.js";
  import Button from "$lib/components/ui/Button.svelte";

  type Props = Extract<State, { state: "connectOrCreatePasskey" }>;

  const { connect, create }: Props = $props();
</script>

<div class="mt-4 mb-8 flex flex-col gap-3" in:fly={{ duration: 200, x: -10 }}>
  <h1
    class="text-gray-light-900 dark:text-gray-dark-25 text-2xl font-medium sm:text-center"
  >
    Simplify your sign-in
  </h1>
  <p
    class="text-md text-gray-light-600 dark:text-gray-dark-50 font-medium sm:text-center sm:text-balance"
  >
    With passkeys, you can now use your fingerprint, face, or screen lock to
    quickly and securely confirm it’s really you.
  </p>
</div>
<div class="mt-auto flex flex-col gap-3">
  <Button onclick={create} size="lg">Set up a new Passkey</Button>
  <Button onclick={connect} variant="secondary" size="lg"
    >Use an existing Passkey</Button
  >
</div>
