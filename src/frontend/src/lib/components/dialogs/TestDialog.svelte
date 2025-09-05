<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { DialogId, dialogsStore } from "$lib/state/dialogs";
  import type { DialogDataMap } from "$lib/state/dialogs";

  const { getDialogData, closeDialog } = dialogsStore;

  const onClose = () => closeDialog(DialogId.Test);
  const data: DialogDataMap[DialogId.Test] = getDialogData(DialogId.Test);
</script>

<Dialog {onClose}>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">Test?</h1>
  <p class="text-text-tertiary mb-8 font-medium">This is a test</p>
  {#if data.test}
    <p class="text-text-tertiary mb-8 font-medium">{data.test}</p>
  {/if}
</Dialog>
