<script lang="ts">
  import { type Snippet } from "svelte";
  let {
    children,
    href,
    hrefAriaLabel = "Link",
    class: classes,
  }: {
    children: Snippet;
    href?: string;
    hrefAriaLabel?: string;
    class?: string;
  } = $props();
</script>

{#if href}
  <a {href} class="contents h-full w-full" aria-label={hrefAriaLabel}>
    <li
      class={`border-border-tertiary text-fg-primary not-disabled:hover:bg-bg-primary_hover disabled:border-border-disabled disabled:text-fg-disabled focus-visible:ring-offset-bg-primary focus-visible:ring-focus-ring flex cursor-pointer items-center gap-2 border-t p-4 text-sm outline-none not-last:border-b last:rounded-b-2xl focus-visible:ring-2 focus-visible:ring-offset-2 ${classes}`}
    >
      {@render children?.()}
    </li>
  </a>
{:else}
  <li
    class={`border-border-tertiary flex items-center gap-2 border-t p-4 not-last:border-b ${classes}`}
  >
    {@render children?.()}
  </li>
{/if}
