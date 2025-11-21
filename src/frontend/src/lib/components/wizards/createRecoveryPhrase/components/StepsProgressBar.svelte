<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  type Props = SvelteHTMLElements["div"] & {
    total: number;
  };

  const { total, class: className, ...props }: Props = $props();
</script>

<div {...props} class={["w-full overflow-hidden", className]}>
  <div class="steps h-1" style="--total: {total}"></div>
</div>

<style>
  .steps {
    background: repeating-linear-gradient(
      to right,
      var(--fg-tertiary),
      var(--fg-tertiary) calc((100% - var(--total) * 16px) / var(--total)),
      transparent calc((100% - var(--total) * 16px) / var(--total)),
      transparent calc((100% - var(--total) * 16px) / var(--total) + 16px)
    );
    animation: translate-animation 100s infinite linear;
    width: calc(100% + 16px);
  }
  @keyframes translate-animation {
    to {
      background-position-x: 10000px;
    }
  }
</style>
