<script lang="ts">
  import Button from "./Button.svelte";
  import CenterCard from "./CenterCard.svelte";
  import BackButton from "./BackButton.svelte";

  let name = $state("");

  const {
    handleCreatePasskey,
  }: {
    handleCreatePasskey: (name: string) => void;
  } = $props();
</script>

<CenterCard>
  <div class="mb-3 flex flex-col gap-3">
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
  </div>
  <Button
    variant="primary"
    disabled={name.length === 0}
    onclick={handleCreatePasskey}>Identify with Passkey</Button
  >
  <BackButton variant="secondary" />
</CenterCard>
