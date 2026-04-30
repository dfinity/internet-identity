<script lang="ts">
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import { CheckIcon, ChevronDownIcon, ChevronUpIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

  /** One row in the picker. `value` is the user-visible string. The
   *  optional `providerLabel` is shown as a secondary tag in the
   *  dropdown when more than one option is available, helping the user
   *  tell same-name values apart by their source. The parent resolves
   *  it (canister-configured for OpenID, two-hop discovery for SSO,
   *  bare issuer/domain otherwise) and passes the final string.
   *  `id` is opaque to this component — used only as the `{#each}`
   *  key so reordering / re-selection doesn't tear DOM nodes. */
  export interface PickerOption {
    id: string;
    value: string;
    providerLabel?: string;
  }

  interface Props {
    label: string;
    options: PickerOption[];
    selectedIndex: number;
    checked: boolean;
    onCheck: (checked: boolean) => void;
    onSelect: (index: number) => void;
    /** Widest label in the surrounding consent view, in pixels. The wrap
     *  probe pads its label slot to this width so the row decides
     *  "wraps?" against the same col 2 width the actual grid layout will
     *  give it — independent of this row's own label length. */
    maxLabelWidth: number;
  }

  const {
    label,
    options,
    selectedIndex,
    checked,
    onCheck,
    onSelect,
    maxLabelWidth,
  }: Props = $props();

  let isOpen = $state(false);

  /** True when the row's `[checkbox] [label] [value] [chevron?]` content
   *  doesn't fit on a single line and the value should drop to its own
   *  row. The probe mirrors the actual row's chrome and stays
   *  `whitespace-nowrap`, so its `scrollWidth` is the natural single-
   *  line width. Comparing to its `clientWidth` (= the picker's content
   *  width) gives a wrap decision that depends only on the picker's
   *  outer width — never on col 2's auto-sizing inside the grid — so
   *  toggling `wrapped` doesn't feed back into the column widths and
   *  trigger oscillation. */
  let wrapped = $state(false);
  let probeEl: HTMLDivElement | undefined = $state();

  const checkWrap = (): void => {
    if (probeEl === undefined) return;
    wrapped = probeEl.scrollWidth > probeEl.clientWidth;
  };

  $effect(() => {
    if (probeEl === undefined) return;
    const ro = new ResizeObserver(checkWrap);
    ro.observe(probeEl);
    return () => ro.disconnect();
  });

  // Re-check when the label or selected value changes (covers locale
  // switches, since the label prop is re-evaluated via `$t` on locale swap).
  $effect(() => {
    // Track the reactive inputs that can change text width.
    void label;
    void options[selectedIndex].value;
    void maxLabelWidth;
    queueMicrotask(checkWrap);
  });

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
  class="border-border-secondary relative col-span-full grid min-w-0 grid-cols-subgrid overflow-hidden rounded-lg border"
  role="group"
  aria-label={label}
>
  <!-- Wrap-detection probe: absolutely-positioned, takes the picker's full
       inner width, mirrors the row's chrome (checkbox + label + value
       + optional chevron) on a single nowrap line. `scrollWidth > clientWidth`
       means the natural line is wider than the picker, so the value should
       drop to its own row. The probe lives outside the grid so its size is
       independent of col-2's auto-sizing — toggling `wrapped` can't feed
       back into the column widths. -->
  <div
    bind:this={probeEl}
    aria-hidden="true"
    class="pointer-events-none invisible absolute inset-x-0 top-0 flex h-0 items-center gap-x-3 overflow-hidden px-3 whitespace-nowrap"
  >
    <span class="size-4 shrink-0"></span>
    <span class="shrink-0 text-sm" style:min-width="{maxLabelWidth}px"
      >{label}</span
    >
    <span class="text-sm font-medium">{options[selectedIndex].value}</span>
    {#if options.length > 1}
      <span class="ms-auto size-6 shrink-0"></span>
    {/if}
  </div>

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
    <!-- When this row's value drops to its own line, span the label across
         cols 2-3 so it doesn't contribute to col-2's `auto`-sizing. Col 2
         then sizes only from rows whose values fit on one line, and those
         shorter rows can left-align their values right after their own
         label. Safe from oscillation because the wrap probe above measures
         independently of col-2 width. -->
    <span
      class={[
        "text-text-secondary row-start-1 my-3 text-sm",
        wrapped ? "col-span-2 col-start-2 me-3" : "col-start-2",
      ]}
    >
      {label}
    </span>

    {#if !wrapped}
      <span
        class={[
          "text-text-primary col-start-3 row-start-1 my-3 min-w-0 overflow-hidden text-sm font-medium text-ellipsis whitespace-nowrap",
          options.length <= 1 && "col-span-2 me-3",
        ]}
      >
        {options[selectedIndex].value}
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
        class="text-text-primary col-span-3 col-start-2 row-start-2 me-3 -mt-2 mb-3 text-sm font-medium break-all"
      >
        {options[selectedIndex].value}
      </span>
    {/if}
  </div>

  {#if isOpen && options.length > 1}
    <div
      class="border-border-secondary group/list col-span-full row-start-2 flex flex-col border-t"
      role="listbox"
      aria-label={label}
    >
      {#each options as option, index (option.id)}
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
            {option.value}
          </span>
          {#if option.providerLabel !== undefined}
            <span class="text-text-tertiary text-sm">
              {option.providerLabel}
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
