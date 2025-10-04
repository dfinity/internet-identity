<script lang="ts">
  type ToggleProps = {
    toggled: boolean;
    onClick: (value: boolean) => void;
    label?: string;
    disabled?: boolean;
  };

  let { toggled, onClick, label, disabled = false }: ToggleProps = $props();
</script>

<label class="flex flex-row items-center gap-2">
  <button
    type="button"
    aria-labelledby={label ? "toggle" : undefined}
    class={`bg-bg-tertiary relative flex h-5 w-9 flex-row items-center rounded-full p-0.5 ${toggled ? "justify-end" : "justify-start"}`}
    onclick={() => onClick(!toggled)}
  >
    <div class="size-4 rounded-full bg-white"></div>
  </button>
  {#if label}
    <span id="toggle-label" class="text-text-secondary text-sm font-medium"
      >{label}</span
    >
  {/if}
  <input
    type="checkbox"
    role="switch"
    aria-labelledby={label ? "toggle" : undefined}
    checked={toggled}
    {disabled}
    onchange={() => onClick(!toggled)}
    hidden
  />
</label>
