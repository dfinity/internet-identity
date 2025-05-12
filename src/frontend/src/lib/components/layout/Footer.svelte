<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";

  type Props = HTMLAttributes<HTMLHeadElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<footer
  {...props}
  class={[
    "flex h-14 items-center justify-between px-4 py-3 md:px-6 lg:px-8",
    className,
  ]}
>
  <div
    class={[
      "text-sm font-medium",
      "text-gray-light-600",
      "dark:text-gray-dark-50",
    ]}
  >
    © Internet Identity
  </div>
  <nav
    class={[
      "flex gap-4 text-sm font-semibold",
      "text-gray-light-900",
      "dark:text-gray-dark-25",
    ]}
  >
    <a href={SUPPORT_URL} target="_blank" rel="noopener">Support</a>
    <a href={SOURCE_CODE_URL} target="_blank" rel="noopener">Source code</a>
  </nav>
</footer>
