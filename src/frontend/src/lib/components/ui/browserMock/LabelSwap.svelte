<script lang="ts">
  // One label replacing another, looping, for a setting a step changes by name
  // rather than with a switch. Both sit in the same grid cell so the control keeps
  // the width of the longer of the two and nothing shifts as they swap.
  const { from, to }: { from: string; to: string } = $props();
</script>

<span class="inline-grid"
  ><span class="out [grid-area:1/1]">{from}</span><span
    class="in opacity-0 [grid-area:1/1]">{to}</span
  ></span
>

<style>
  @media (prefers-reduced-motion: no-preference) {
    .out {
      animation: out 3s ease-in-out infinite;
    }
    .in {
      animation: in 3s ease-in-out infinite;
    }
  }

  @keyframes out {
    0%,
    35% {
      opacity: 1;
    }
    45%,
    90% {
      opacity: 0;
    }
    100% {
      opacity: 1;
    }
  }

  @keyframes in {
    0%,
    35% {
      opacity: 0;
    }
    45%,
    90% {
      opacity: 1;
    }
    100% {
      opacity: 0;
    }
  }
</style>
