<script lang="ts">
  import { goto } from "$app/navigation";
  import { page } from "$app/state";
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import { AddPasskeyFlow } from "$lib/flows/addPasskeyFlow.svelte";
  import { onMount } from "svelte";

  const user = page.url.searchParams.get("user");
  const flow = new AddPasskeyFlow(BigInt(user!));

  const handleAddPasskey = async () => {
    const addPasskeyResult = await flow.addPasskey();

    if ("Ok" in addPasskeyResult) {
      goto("/manage");
    }
  };

  onMount(() => {
    flow.addTemporaryKey();
  });
</script>

<div class="flex h-screen w-screen items-center justify-center">
  <Panel class="p-4">
    <h1 class="text-text-primary mb-3.5 text-2xl font-semibold">
      Add Device Flow
    </h1>
    {#if flow.view === "loading"}
      <p class="text-text-secondary">Loading...</p>
    {:else if flow.view === "show-code"}
      <p class="text-text-secondary mb-2">Authorization code</p>
      <p class="text-text-secondary">{flow.verificationCode}</p>
    {:else if flow.view === "add-device"}
      <p class="text-text-secondary mb-2">Add device</p>
      <Button onclick={handleAddPasskey}>Add passkey</Button>
    {/if}
  </Panel>
</div>
