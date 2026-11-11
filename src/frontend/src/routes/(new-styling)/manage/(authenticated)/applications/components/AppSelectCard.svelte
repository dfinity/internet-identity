<script lang="ts">
  import { CheckIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import AppSummary from "./AppSummary.svelte";
  import { appNameStore } from "../apps";

  interface Props {
    origin: string;
    selected: boolean;
    onToggle: () => void;
  }

  const { origin, selected, onToggle }: Props = $props();

  const nameStore = $derived(appNameStore(origin));
  const name = $derived($nameStore);
</script>

<button
  role="checkbox"
  aria-checked={selected}
  aria-label={selected ? $t`Deselect ${name}` : $t`Select ${name}`}
  onclick={onToggle}
  class="block w-full cursor-pointer text-start outline-none"
>
  <span
    class="bg-bg-secondary grid grid-cols-[auto_minmax(0,1fr)_auto] items-center gap-x-3.5 py-4 ps-4 pe-3"
  >
    <AppSummary {origin} />
    <span class="inline-flex size-9 items-center justify-center">
      <span
        class={[
          "text-fg-primary-inversed inline-flex size-5 shrink-0 items-center justify-center rounded-sm",
          selected
            ? "bg-bg-brand-solid"
            : "border-border-primary bg-bg-primary border",
        ]}
      >
        {#if selected}
          <CheckIcon class="size-4" strokeWidth={3.2} />
        {/if}
      </span>
    </span>
  </span>
  <span
    class="bg-bg-primary border-border-tertiary flex min-h-13 items-center gap-3 border-t py-2 ps-4 pe-3"
  >
    <span class="text-text-tertiary text-[13px] leading-[18px]">
      {selected ? $t`Will be removed` : $t`Tap to select`}
    </span>
  </span>
</button>
