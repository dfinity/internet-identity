<script lang="ts">
  import { type Snippet } from "svelte";
  let {
    colOne,
    colTwo,
    colThree,
    gridCols = "0.5fr 1fr min-content",
    href,
    hrefAriaLabel = "Link",
  }: {
    colOne: Snippet;
    colTwo: Snippet;
    colThree: Snippet;
    gridCols?: string;
    href?: string;
    hrefAriaLabel?: string;
  } = $props();

  let tailwindGridCols = $derived(gridCols.replaceAll(" ", "_"));
</script>

{#if href}
  <a {href} class="contents h-full w-full" aria-label={hrefAriaLabel}>
    <li
      class={`border-border-tertiary text-fg-primary not-disabled:hover:bg-bg-primary_hover disabled:border-border-disabled disabled:text-fg-disabled focus-visible:ring-offset-bg-primary focus-visible:ring-focus-ring grid cursor-pointer grid-cols-[${tailwindGridCols}] items-center border-t p-4 text-sm outline-none not-last:border-b last:rounded-b-2xl focus-visible:ring-2 focus-visible:ring-offset-2`}
    >
      <div class="col-start-1 col-end-2">
        {@render colOne?.()}
      </div>
      <div class="col-start-2 col-end-3">
        {@render colTwo?.()}
      </div>
      <div class="col-start-3 col-end-4">
        {@render colThree?.()}
      </div>
    </li>
  </a>
{:else}
  <li
    class={`border-border-tertiary grid grid-cols-[${tailwindGridCols}] items-center border-t p-4 not-last:border-b`}
  >
    <div class="col-start-1 col-end-2">
      {@render colOne?.()}
    </div>
    <div class="col-start-2 col-end-3">
      {@render colTwo?.()}
    </div>
    <div class="col-start-3 col-end-4">
      {@render colThree?.()}
    </div>
  </li>
{/if}
