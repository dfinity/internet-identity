<script lang="ts">
  import { Edit } from "@lucide/svelte";
  import Input from "./Input.svelte";
  import Button from "./Button.svelte";
  import PlaceHolder from "./PlaceHolder.svelte";

  type Props = {
    name: string;
    // onSave should handle the error.
    onSave: (name: string) => Promise<void>;
  };

  let { name, onSave } = $props();

  let isEditing = $state(false);
  let isLoading = $state(false);
  let inputValue = $state(name);
  let inputElement = $state<HTMLInputElement>();

  const startEditing = () => {
    inputValue = name;
    isEditing = true;

    // Focus the input after the DOM updates
    setTimeout(() => {
      if (inputElement) {
        inputElement.focus();
      }
    }, 0);
  };

  const handleSave = async () => {
    if (!isEditing || inputValue === name) {
      isEditing = false;
      return;
    }

    isEditing = false;
    isLoading = true;
    await onSave(inputValue);
    isLoading = false;
  };

  const handleKeyDown = (event: KeyboardEvent) => {
    if (event.key === "Enter") {
      handleSave();
    } else if (event.key === "Escape") {
      inputValue = name;
      isEditing = false;
    }
  };
</script>

<div class="inline-flex items-center gap-1">
  {#if isEditing}
    <Input
      bind:element={inputElement}
      bind:value={inputValue}
      onblur={handleSave}
      onkeydown={handleKeyDown}
      type="text"
      disabled={isLoading}
      size="sm"
    />
  {:else if isLoading}
    <PlaceHolder class="w-30" />
  {:else}
    <span class="name-display">{name}</span>
    <Button
      variant="tertiary"
      iconOnly
      onclick={startEditing}
      aria-label="Edit name"
    >
      <Edit size={16} />
    </Button>
  {/if}
</div>
