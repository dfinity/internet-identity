<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";
  import { type State } from "../state";

  interface Props {
    pick: (method: "passkey" | "google") => void;
    cancel: () => void;
  }

  const { pick, cancel }: Props = $props();
</script>

<div class="flex flex-col items-stretch gap-4">
  <Button onclick={() => pick("passkey")} variant="primary"
    >Continue with Passkey</Button
  >
  <Button onclick={() => pick("google")} variant="secondary"
    >Continue with Google</Button
  >
  <Button onclick={cancel} class="w-full" variant="text-only">Cancel</Button>
</div>
