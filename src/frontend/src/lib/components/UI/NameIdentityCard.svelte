<script lang="ts">
  import Button from "./Button.svelte";

  let name = $state("");

  const {
    handleCreateIdentity,
    close,
    class: classes,
  }: {
    handleCreateIdentity: (name: string) => void;
    close: () => void;
    class: string;
  } = $props();
</script>

<div class={`flex flex-col gap-3 p-4 ${classes}`}>
  <h1 class="h1 font-bold">[Name your identity]</h1>
  <label class="label">
    <!-- <span class="label-text">Label</span> -->
    <input
      bind:value={name}
      type="text"
      class="input px-4 py-2"
      placeholder="Enter Name"
    />
  </label>
  <Button
    variant="primary"
    disabled={name.length === 0}
    onclick={handleCreateIdentity}>Identify with Passkey</Button
  >
  <Button onclick={close} variant="secondary">Back</Button>
</div>
