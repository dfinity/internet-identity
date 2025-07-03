<script lang="ts">
  import { page } from "$app/state";
  import { AddPasskeyFlow } from "$lib/flows/addPasskeyFlow.svelte";
  import { onMount } from "svelte";

  const user = page.url.searchParams.get("user");
  const flow = new AddPasskeyFlow(BigInt(user!));

  onMount(() => {
    flow.addTemporaryKey();
  });
</script>

<h1 class="text-text-primary">Add Device Flow</h1>
{#if flow.view === "loading"}
  <p class="text-text-primary">Loading...</p>
{:else if flow.view === "show-code"}
  <p class="text-text-primary">Authorization code</p>
  <p class="text-text-primary">{flow.verificationCode}</p>
{:else if flow.view === "add-device"}
  <p class="text-text-primary">Add device</p>
{/if}
