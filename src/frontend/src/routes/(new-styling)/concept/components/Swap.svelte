<script lang="ts">
  /**
   * What waits for data: a skeleton as large as the block until the data
   * lands, then the block, the two crossing in one grid cell so nothing
   * around them moves (loading.css). After Fluid Functionalism's Reveal
   * (`.reveal-swap`). Each Swap is where the product waits for a request.
   */
  import type { Snippet } from "svelte";
  import { quintOut } from "svelte/easing";
  import type { ClassValue } from "svelte/elements";
  import { prefersReducedMotion } from "svelte/motion";
  import { fade } from "svelte/transition";
  import type { LoadState } from "../loading.svelte";
  import { arrive } from "../motion";

  interface Props {
    status: LoadState;
    /** Stands where the block will be, as large as it, while it loads. */
    skeleton: Snippet;
    /** In the block's place if its data never came. */
    failed?: Snippet;
    /** The block's rows, which arrive one by one (`arrive`), by selector;
     *  otherwise the block fades in whole. */
    items?: string;
    class?: ClassValue;
    children: Snippet;
  }

  const {
    status,
    skeleton,
    failed,
    items,
    class: className,
    children,
  }: Props = $props();

  const reduced = $derived(prefersReducedMotion.current);

  // The block arrives only where a skeleton stood: one there from the start
  // comes with the page (`arrive` on the page's blocks).
  // svelte-ignore state_referenced_locally
  let waited = $state(status !== "ready");
  $effect.pre(() => {
    if (status !== "ready") {
      waited = true;
    }
  });
</script>

<div class={["ii-swap", className]} aria-busy={status === "loading"}>
  {#if status === "loading"}
    <div
      aria-hidden="true"
      class="pointer-events-none"
      out:fade={{ duration: reduced ? 100 : 200, easing: quintOut }}
    >
      {@render skeleton()}
    </div>
  {/if}
  {#if status === "failed" && failed !== undefined}
    <div in:fade={{ duration: reduced ? 150 : 200, easing: quintOut }}>
      {@render failed()}
    </div>
  {/if}
  {#if status === "ready"}
    <div
      use:arrive={{ step: 30, targets: items, reduced, skip: !waited }}
      class="min-w-0"
    >
      {@render children()}
    </div>
  {/if}
</div>
