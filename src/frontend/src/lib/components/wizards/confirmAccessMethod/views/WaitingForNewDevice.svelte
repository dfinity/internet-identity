<script lang="ts">
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
</script>

<div class="flex flex-col">
  <div class="flex h-40 items-center justify-center">
    <ProgressRing class="text-fg-primary size-14" />
  </div>
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    Waiting for your new device
  </h1>
  <p class="text-md text-text-tertiary font-medium text-balance sm:text-center">
    The <b class="text-text-primary">new device</b> is preparing to continue.
  </p>
</div>
