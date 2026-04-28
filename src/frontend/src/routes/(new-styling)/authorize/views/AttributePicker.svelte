<script lang="ts">
  import type { AvailableAttribute } from "$lib/stores/attributeConsent.store";
  import { extractScope } from "$lib/stores/channelHandlers/attributes";
  import { backendCanisterConfig } from "$lib/globals";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import { CheckIcon, ChevronDownIcon, ChevronUpIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    label: string;
    options: AvailableAttribute[];
    selectedIndex: number;
    checked: boolean;
    onCheck: (checked: boolean) => void;
    onSelect: (index: number) => void;
  }

  const { label, options, selectedIndex, checked, onCheck, onSelect }: Props =
    $props();

  let isOpen = $state(false);

  /** True when the value doesn't fit in col 3 and should move below the
   *  label on its own row. Decided by an invisible probe span kept in col 3
   *  row 1 regardless of layout state; we compare its natural text width
   *  (scrollWidth) to the column's rendered width (clientWidth). A
   *  ResizeObserver on the row keeps this reactive to viewport changes,
   *  locale changes, and value changes. */
  let wrapped = $state(false);
  let rowEl: HTMLDivElement | undefined = $state();
  let probeEl: HTMLSpanElement | undefined = $state();

  const checkWrap = (): void => {
    if (probeEl === undefined) return;
    wrapped = probeEl.scrollWidth > probeEl.clientWidth;
  };

  $effect(() => {
    if (rowEl === undefined) return;
    const ro = new ResizeObserver(checkWrap);
    ro.observe(rowEl);
    return () => ro.disconnect();
  });

  // Re-check when the label or selected value changes (covers locale
  // switches, since the label prop is re-evaluated via `$t` on locale swap).
  $effect(() => {
    // Track the reactive inputs that can change text width.
    void label;
    void options[selectedIndex].displayValue;
    queueMicrotask(checkWrap);
  });

  const getProviderName = (key: string): string | undefined => {
    const scope = extractScope(key);
    if (scope === undefined || !scope.startsWith("openid:")) {
      return undefined;
    }
    const issuer = scope.slice("openid:".length);
    return backendCanisterConfig.openid_configs[0]?.find(
      (c) => c.issuer === issuer,
    )?.name;
  };

  /** Toggle the dropdown when the click landed on a non-interactive part of
   *  the row (i.e. not on the checkbox, chevron, or a dropdown option). The
   *  chevron button stays focusable for keyboard users; the row click is a
   *  mouse-only convenience for a larger hit target. */
  const handleRowClick = (e: MouseEvent): void => {
    if (options.length <= 1) return;
    const target = e.target as HTMLElement | null;
    if (target?.closest("button, input, label") !== null) return;
    isOpen = !isOpen;
  };
</script>

<div
  bind:this={rowEl}
  class="border-border-secondary col-span-full grid min-w-0 grid-cols-subgrid overflow-hidden rounded-lg border"
  role="group"
  aria-label={label}
>
  <!-- Row-1 wrapper owns the hover/click, so the background doesn't bleed
       into the dropdown area below. Keyboard users toggle via the focusable
       chevron button; this click handler is mouse-only convenience. -->
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div
    class={[
      "col-span-full row-start-1 grid grid-cols-subgrid items-center gap-x-3",
      options.length > 1 && "hover:bg-bg-primary_hover cursor-pointer",
    ]}
    onclick={handleRowClick}
  >
    <Checkbox
      {checked}
      onchange={() => onCheck(!checked)}
      size="sm"
      class="col-start-1 row-start-1 my-3 ms-3"
      aria-label={$t`Share ${label}`}
    />
    <span class="text-text-secondary col-start-2 row-start-1 my-3 text-sm"
      >{label}</span
    >

    <!-- Invisible probe: always sits in col 3 (matching the unwrapped value's
         cell), so `probeEl.scrollWidth > probeEl.clientWidth` tells us
         whether the value would overflow regardless of wrap state. -->
    <span
      bind:this={probeEl}
      aria-hidden="true"
      class={[
        "invisible col-start-3 row-start-1 h-0 overflow-hidden text-sm font-medium whitespace-nowrap",
        options.length <= 1 && "col-span-2 me-3",
      ]}
    >
      {options[selectedIndex].displayValue}
    </span>

    {#if !wrapped}
      <span
        class={[
          "text-text-primary col-start-3 row-start-1 my-3 min-w-0 overflow-hidden text-sm font-medium text-ellipsis whitespace-nowrap",
          options.length <= 1 && "col-span-2 me-3",
        ]}
      >
        {options[selectedIndex].displayValue}
      </span>
    {/if}

    {#if options.length > 1}
      {#if isOpen}
        <button
          onclick={() => (isOpen = false)}
          class="btn btn-tertiary btn-sm btn-icon col-start-4 row-start-1 my-3 me-3 size-6! shrink-0 justify-self-end"
          aria-label={$t`Close`}
          aria-expanded="true"
        >
          <ChevronUpIcon class="size-4" />
        </button>
      {:else}
        <button
          onclick={() => (isOpen = true)}
          class="btn btn-tertiary btn-sm btn-icon col-start-4 row-start-1 my-3 me-3 size-6! shrink-0 justify-self-end"
          aria-label={$t`Change`}
          aria-expanded="false"
        >
          <ChevronDownIcon class="size-4" />
        </button>
      {/if}
    {/if}

    {#if wrapped}
      <span
        class="text-text-primary col-span-3 col-start-2 row-start-2 me-3 mb-3 text-sm font-medium break-all"
      >
        {options[selectedIndex].displayValue}
      </span>
    {/if}
  </div>

  {#if isOpen && options.length > 1}
    <div
      class="border-border-secondary group/list col-span-full row-start-2 flex flex-col border-t"
      role="listbox"
      aria-label={label}
    >
      {#each options as option, index (option.key)}
        {@const providerName = getProviderName(option.key)}
        <button
          onclick={() => {
            onSelect(index);
            isOpen = false;
          }}
          class={[
            "flex flex-row items-center gap-3 p-3 text-start",
            index === selectedIndex
              ? "bg-bg-active hover:bg-bg-active! group-hover/list:bg-transparent"
              : "hover:bg-bg-primary_hover",
          ]}
          role="option"
          aria-selected={index === selectedIndex}
        >
          <span class="text-text-primary text-sm font-medium">
            {option.displayValue}
          </span>
          {#if providerName !== undefined}
            <span class="text-text-tertiary text-sm">
              {providerName}
            </span>
          {/if}
          {#if index === selectedIndex}
            <CheckIcon
              class="text-fg-tertiary ms-auto me-1 size-4 shrink-0"
              aria-hidden="true"
            />
          {/if}
        </button>
      {/each}
    </div>
  {/if}
</div>
