<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { DialogId, dialogsStore } from "$lib/state/dialogs";
  import type { DialogDataMap } from "$lib/state/dialogs";
  import Button from "../ui/Button.svelte";

  const { getDialogData, closeDialog } = dialogsStore;

  const onClose = () => closeDialog(DialogId.Test2);
  const data: DialogDataMap[DialogId.Test2] = getDialogData(DialogId.Test2);
</script>

<Dialog {onClose}>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">Test 2?</h1>
  <p class="text-text-tertiary mb-8 font-medium">This is a second test</p>
  {#if data.callback}
    <Button onclick={data.callback}>Click me!</Button>
  {/if}
</Dialog>
