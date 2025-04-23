<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";
  import { fly } from "svelte/transition";
  import { State } from "../+page.svelte";

  type Props = Extract<State, { state: "connectOrCreatePasskey" }>;

  const { connect, create }: Props = $props();
</script>

<div class="mb-8 flex flex-col gap-4" in:fly={{ duration: 200, x: -10 }}>
  <p>
    Lorem ipsum dolor sit amet consectetur, adipisicing elit. Doloribus alias
    amet quas, ducimus iste iure et.
  </p>
  <p>
    Boriosam aliquid rerum dolore porro optio, explicabo corrupti temporibus
    consequuntur quae accusamus eligendi eius.
  </p>
</div>
<div class="mt-auto flex flex-col items-stretch gap-4">
  <Button onclick={connect} variant="primary">Connect my Passkey</Button>
  <Button onclick={create} variant="secondary"
    >Don't have a passkey? Create one</Button
  >
</div>
