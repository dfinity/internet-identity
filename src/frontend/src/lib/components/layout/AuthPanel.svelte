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
    "grid w-full flex-col rounded-xl px-4 pt-5 pb-8",
    "max-sm:flex-1",
    "sm:from-bg-secondary sm:via-bg-secondary sm:to-bg-secondary/97 sm:bg-orange-400 sm:bg-gradient-to-t sm:px-6 sm:ring sm:ring-black/10 sm:ring-inset dark:sm:bg-blue-400 dark:sm:ring-white/14",
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
