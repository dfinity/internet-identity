<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { fly } from "svelte/transition";
  import { page, navigating } from "$app/state";

  type Props = HTMLAttributes<HTMLDivElement>;

  const { children, class: className, ...props }: Props = $props();

  let divRef = $state<HTMLElement>();
</script>

<div
  {...props}
  class={[
    "grid w-full flex-col overflow-hidden rounded-xl px-4 pt-5 pb-8",
    "max-sm:flex-1",
    "sm:bg-bg-secondary sm:border-border-secondary sm:border sm:px-6",
    className,
  ]}
>
  {#if !("disableNavigationAnimation" in page.state)}
    {#key page.route.id}
      <div
        bind:this={divRef}
        class="col-start-1 row-start-1 flex flex-col max-sm:flex-1"
        in:fly={{
          x: 200 * (navigating.delta ?? 1),
          duration: 200,
          delay: 80,
        }}
        out:fly={{ x: -160 * (navigating.delta ?? 1), duration: 160 }}
        onoutrostart={() => divRef?.setAttribute("aria-hidden", "true")}
      >
        {@render children?.()}
      </div>
    {/key}
  {:else}
    {@render children?.()}
  {/if}
</div>
