<script lang="ts">
  import { onDestroy } from "svelte";
  import { derived } from "svelte/store";
  import TestDialog from "./TestDialog.svelte";
  import { DialogId, dialogsStore } from "$lib/state/dialogs";
  import TestDialog2 from "./TestDialog2.svelte";

  const dialogs = derived(dialogsStore, ($store) => $store);

  let activeDialogs: Partial<Record<DialogId, { isOpen: boolean }>> = {};

  const unsubscribe = dialogs.subscribe((d) => (activeDialogs = d));
  onDestroy(unsubscribe);
</script>

{#if activeDialogs[DialogId.Test]?.isOpen}
  <TestDialog />
{/if}
{#if activeDialogs[DialogId.Test2]?.isOpen}
  <TestDialog2 />
{/if}
