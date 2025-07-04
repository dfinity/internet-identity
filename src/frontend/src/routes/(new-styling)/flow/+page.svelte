<script lang="ts">
  import { goto } from "$app/navigation";
  import { page } from "$app/state";
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import { AddPasskeyFlow } from "$lib/flows/addPasskeyFlow.svelte";
  import { onMount } from "svelte";

  const user = page.url.searchParams.get("user");
  if (!user) goto("/");
  const flow = new AddPasskeyFlow(BigInt(user!));

  const handleAddPasskey = async () => {
    const addPasskeyResult = await flow.addPasskey();

    if ("Ok" in addPasskeyResult) {
      goto("/");
    }
  };

  onMount(() => {
    flow.addTemporaryKey();
  });
</script>

<div class="bg-bg-secondary flex h-screen w-screen items-center justify-center">
  <Panel class="p-4">
    <h1 class="text-text-primary mb-3.5 text-2xl font-semibold">
      Add a new passkey to your identity
    </h1>
    <div>
      {#if flow.view === "loading"}
        <p class="text-text-secondary text-center">Loading...</p>
      {:else if flow.view === "show-code"}
        <p class="text-text-secondary mb-3.5">
          Please enter the code displayed below on your original device.
        </p>
        <p class="text-text-primary text-center text-4xl">
          {flow.verificationCode}
        </p>
      {:else if flow.view === "add-device"}
        <p class="text-text-secondary mb-4 text-center nth-[2]:hidden">
          Please add a new passkey to use on this device.
        </p>
        <div class="w-full">
          <Button class="w-full" onclick={handleAddPasskey}>Add passkey</Button>
        </div>
      {/if}
    </div>
  </Panel>
</div>
