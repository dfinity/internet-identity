<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";
  import { type State } from "../../state";

  type Props = Omit<Extract<State, { state: "authenticate" }>, "state">;

  const { pick }: Props = $props();
</script>

<div class="flex flex-col items-stretch gap-4">
  <Button onclick={() => pick("passkey")} variant="primary"
    >Continue with Passkey</Button
  >
  <Button onclick={() => pick("google")} variant="secondary"
    >Continue with Google</Button
  >
  <Button class="w-full" variant="text-only">Cancel</Button>
</div>
