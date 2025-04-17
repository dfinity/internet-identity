<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";
  import { type State } from "../state";

  type Props = Extract<State, { state: "continueAs" }>;

  const { number, name, credentialId, continue: continueFn, useAnother }: Props = $props();
</script>

<div class="flex flex-col items-stretch gap-4">
  <Button onclick={() => continueFn(credentialId)} class="px-6 py-4 text-left" variant="primary"
    >Continue as {name ?? number}</Button
  >
  <Button onclick={useAnother} class="px-6 py-4 text-left" variant="dashed"
    >Use another Internet Identity</Button
  >
</div>
