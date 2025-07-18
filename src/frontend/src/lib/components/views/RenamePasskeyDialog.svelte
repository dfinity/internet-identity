<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import { EditIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import type { AuthnMethodData } from "$lib/generated/internet_identity_types";

  interface Props {
    currentName: string;
    onRename: (newName: string) => Promise<void>;
    onClose: () => void;
  }

  const { currentName, onRename, onClose }: Props = $props();

  let inputValue = $state(currentName);
  let isLoading = $state(false);
  let inputElement = $state<HTMLInputElement>();

  const handleSave = async (e: SubmitEvent) => {
    e.preventDefault();
    // Button is disabled so this shouldn't happen.
    if (inputValue.trim() === "") {
      return;
    }

    isLoading = true;

    try {
      await onRename(inputValue.trim());
      onClose();
    } finally {
      isLoading = false;
    }
  };

  // Focus the input when the dialog opens and auto-select the text
  $effect(() => {
    if (inputElement) {
      inputElement.select();
    }
  });
</script>

<Dialog onClose={isLoading ? undefined : onClose}>
  <FeaturedIcon variant="info" size="lg" class="mb-3">
    <EditIcon size="1.5rem" />
  </FeaturedIcon>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">Rename passkey</h1>
  <p class="text-text-tertiary mb-6 font-medium">
    Give your passkey a memorable name to help you identify it.
  </p>

  <form onsubmit={handleSave} class="flex flex-col gap-6">
    <Input
      bind:element={inputElement}
      bind:value={inputValue}
      type="text"
      disabled={isLoading}
      class="w-full"
      autofocus
    />
    <div class="flex flex-col gap-3">
      <Button
        type="submit"
        variant="primary"
        disabled={isLoading || inputValue.trim() === ""}
      >
        {isLoading ? "Saving..." : "Save"}
      </Button>
      <Button onclick={onClose} variant="tertiary" disabled={isLoading}>
        Cancel
      </Button>
    </div>
  </form>
</Dialog>
