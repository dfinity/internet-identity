<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Logo from "$lib/components/Logo.svelte";

  type Props = HTMLAttributes<HTMLHeadElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<header
  {...props}
  class={["flex h-14 items-center px-4 py-3 md:px-6 lg:px-8", className]}
>
  <!-- TODO: Revert to anchor tag when we create the landing page -->
  <div class="flex h-14 items-center gap-4">
    <Logo class="text-fg-primary h-5.5" />
    <h1 class="text-md text-text-primary font-semibold md:block">
      Internet Identity
    </h1>
  </div>
</header>
