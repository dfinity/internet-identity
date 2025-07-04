<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { SUPPORT_URL } from "$lib/config";

  type Props = HTMLAttributes<HTMLElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<footer
  {...props}
  class={[
    "flex h-14 items-center justify-between px-4 py-3 md:px-6 lg:px-8",
    className,
  ]}
>
  <div class="text-text-tertiary text-sm font-medium">© Internet Identity</div>
  <nav class="text-text-primary flex gap-4 text-sm font-semibold">
    <a href={SUPPORT_URL} target="_blank" rel="noopener">Support</a>
  </nav>
</footer>
