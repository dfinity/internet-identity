<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Logo from "$lib/components/ui/Logo.svelte";

  type Props = HTMLAttributes<HTMLElement>;
  const { children, class: className, ...props }: Props = $props();
</script>

<header
  {...props}
  class={[
    "to-bg-primary from-bg-primary flex items-center bg-gradient-to-b px-0 md:to-transparent md:px-6 lg:px-8",
    className,
  ]}
>
  <div class="flex min-h-16 flex-1 items-center gap-4">
    <Logo class="text-fg-primary h-5.5" />
    <h1 class="text-md text-text-primary hidden font-semibold sm:block">
      Internet Identity
    </h1>
  </div>
  {@render children?.()}
</header>
