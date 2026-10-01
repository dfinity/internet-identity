<script lang="ts">
  interface Props {
    /** Animates from off to on and back, so the step shows the move it asks for. */
    animate?: boolean;
  }

  const { animate = false }: Props = $props();
</script>

<span
  class={[
    "relative inline-flex h-3 w-6 shrink-0 items-center rounded-full",
    animate ? "mock-track" : "bg-bg-brand-solid",
  ]}
>
  <span
    class={[
      "absolute size-2.5 rounded-full",
      animate ? "mock-knob" : "translate-x-[13px] bg-white",
    ]}
  ></span>
</span>

<style>
  /* The move the step asks for, played out: the track takes the brand colour as the
     knob travels. Decorative, so it stops for a reader who asked it to. */
  .mock-track {
    background: var(--bg-tertiary);
    animation: mock-track 3s ease-in-out infinite;
  }
  .mock-knob {
    left: 2px;
    background: var(--text-primary);
    animation: mock-knob 3s ease-in-out infinite;
  }
  @keyframes mock-track {
    0%,
    35% {
      background: var(--bg-tertiary);
    }
    45%,
    90% {
      background: var(--bg-brand-solid);
    }
    100% {
      background: var(--bg-tertiary);
    }
  }
  @keyframes mock-knob {
    0%,
    35% {
      transform: translateX(0);
      background: var(--text-primary);
    }
    45%,
    90% {
      transform: translateX(13px);
      background: white;
    }
    100% {
      transform: translateX(0);
      background: var(--text-primary);
    }
  }
  @media (prefers-reduced-motion: reduce) {
    .mock-track,
    .mock-knob {
      animation: none;
    }
    .mock-knob {
      transform: translateX(13px);
      background: white;
    }
    .mock-track {
      background: var(--bg-brand-solid);
    }
  }
</style>
