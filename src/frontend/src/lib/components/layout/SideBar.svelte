<script lang="ts">
  import Logo from "../ui/Logo.svelte";

  const { children } = $props();
</script>

<nav
  class="bg-bg-primary border-border-secondary min-h-screen max-w-96 border-r px-3"
>
  <div class="flex h-16 flex-1 items-center gap-4">
    <Logo class="text-fg-primary h-5.5" />
    <h1 class="text-md text-text-primary hidden font-semibold sm:block">
      Internet Identity Hub
    </h1>
  </div>
  <div>
    {@render children?.()}
  </div>
</nav>
