<script lang="ts">
  interface Props {
    /** Plays the move the step asks for, so the switch is shown being turned on. */
    animate?: boolean;
  }

  const { animate = false }: Props = $props();
</script>

<div
  class={[
    "bg-surface-light-300 dark:bg-surface-dark-600 relative h-4 w-7 rounded-full",
    animate && "mock-track",
  ]}
>
  <div
    class={[
      "bg-text-primary absolute top-0.5 left-0.5 size-3 rounded-full",
      animate ? "mock-knob" : "translate-x-3",
    ]}
  ></div>
</div>

<style>
  .mock-track {
    animation: mock-track 3s ease-in-out infinite;
  }
  .mock-knob {
    animation: mock-knob 3s ease-in-out infinite;
  }
  @keyframes mock-track {
    0%,
    35% {
      background: var(--color-surface-dark-600);
    }
    45%,
    90% {
      background: var(--bg-brand-solid);
    }
    100% {
      background: var(--color-surface-dark-600);
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
      transform: translateX(12px);
      background: var(--text-primary-inversed);
    }
    100% {
      transform: translateX(0);
      background: var(--text-primary);
    }
  }
  /* Still, the switch is shown off: that is the state the step asks the user to
     find, and nothing here moves to show them the rest. */
  @media (prefers-reduced-motion: reduce) {
    .mock-track,
    .mock-knob {
      animation: none;
    }
  }
</style>
