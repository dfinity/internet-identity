<script lang="ts">
  import { Edit } from "@lucide/svelte";
  import Input from "./Input.svelte";
  import Button from "./Button.svelte";
  import PlaceHolder from "./PlaceHolder.svelte";

  type Props = {
    text: string;
    // onSave should handle the error.
    onSave: (text: string) => Promise<void>;
  };

  let { text, onSave }: Props = $props();

  let isEditing = $state(false);
  let isLoading = $state(false);
  let inputValue = $state(text);
  let inputElement = $state<HTMLInputElement>();

  const startEditing = () => {
    inputValue = text;
    isEditing = true;

    // Focus the input after the DOM updates
    setTimeout(() => {
      if (inputElement) {
        inputElement.focus();
      }
    }, 0);
  };

  const handleSave = async () => {
    if (!isEditing || inputValue === text) {
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
      inputValue = text;
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
    <span class="name-display">{text}</span>
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
