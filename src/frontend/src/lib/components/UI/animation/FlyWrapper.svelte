<script lang="ts">
  import { fly } from "svelte/transition";
  const { children, delay, handleTransitionEnd } = $props<{
    children: any;
    delay?: number;
    handleTransitionEnd?: () => void;
  }>();
</script>

<div
  in:fly={{
    duration: 300,
    delay,
    x: 200,
  }}
  out:fly={{
    duration: 300,
    x: -200,
  }}
  onoutroend={handleTransitionEnd}
>
  {@render children()}
</div>
