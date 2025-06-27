<script lang="ts">
  import { type Snippet } from "svelte";
  import Logo from "$lib/components/ui/Logo.svelte";

  const { children, class: classes }: { children: Snippet; class?: string } =
    $props();
</script>

<nav
  class={["bg-bg-primary border-border-secondary border-r px-4 py-5", classes]}
>
  <div class="mb-6 flex flex-1 items-center gap-4 px-3">
    <Logo class="text-fg-primary h-5.5" />
    <h1 class="text-md text-text-primary font-semibold sm:block">
      Internet Identity <span class="font-medium">Hub</span>
    </h1>
  </div>
  <div class="flex flex-col justify-start">
    {@render children?.()}
  </div>
</nav>
