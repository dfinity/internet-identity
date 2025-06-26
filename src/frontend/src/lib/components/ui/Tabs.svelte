<script lang="ts">
  import Logo from "./Logo.svelte";

  const { children } = $props();
</script>

<nav class="bg-bg-primary_alt mb-4 px-4 py-3">
  <div class="mb-6 flex gap-2">
    <Logo class="text-fg-primary h-5.5" />
    <h1 class="text-text-primary font-semibold sm:block">Hub</h1>
  </div>
  {@render children?.()}
</nav>
