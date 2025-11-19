<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Logo from "$lib/components/ui/Logo.svelte";

  type Props = HTMLAttributes<HTMLElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<header
  {...props}
  class={[
    "flex items-center bg-gradient-to-b from-transparent to-transparent px-3 md:px-6 lg:px-8",
    className,
  ]}
>
  <div class="flex h-16 flex-1 items-center gap-4">
    <a href="/" class="flex items-center gap-4">
      <Logo class="text-fg-primary h-5.5" />
      <h1 class="text-text-primary hidden text-base font-semibold sm:block">
        Internet Identity
      </h1>
    </a>
    {@render children?.()}
  </div>
</header>
