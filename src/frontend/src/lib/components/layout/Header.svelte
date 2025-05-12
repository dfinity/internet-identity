<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Logo from "$lib/components/ui/Logo.svelte";

  type Props = HTMLAttributes<HTMLHeadElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<header
  {...props}
  class={[
    "flex h-14 items-center border-b px-4 py-3 md:px-6 lg:px-8",
    "border-gray-light-200",
    "dark:border-gray-light-700",
    className,
  ]}
>
  <a href="/" class="flex h-14 items-center gap-4">
    <Logo class={["h-5.5", "text-gray-light-900", "dark:text-gray-dark-25"]} />
    <h1
      class={[
        "text-md hidden font-semibold md:block",
        "text-gray-light-900",
        "dark:text-gray-dark-25",
      ]}
    >
      Internet Identity
    </h1>
  </a>
</header>
