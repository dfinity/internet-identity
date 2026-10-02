<!-- An off switch being turned on, looping, so the step shows the move it asks for. -->
<div
  class="bg-surface-light-300 dark:bg-surface-dark-600 track relative h-4 w-7 rounded-full"
>
  <div
    class="bg-text-primary knob absolute top-0.5 left-0.5 h-3 w-3 rounded-full"
  ></div>
</div>

<style>
  @media (prefers-reduced-motion: no-preference) {
    .track {
      animation: track 3s ease-in-out infinite;
    }
    .knob {
      animation: knob 3s ease-in-out infinite;
    }
  }

  @keyframes track {
    0%,
    35% {
      background: var(--color-surface-light-300);
    }
    45%,
    90% {
      background: var(--bg-brand-solid);
    }
    100% {
      background: var(--color-surface-light-300);
    }
  }

  @keyframes knob {
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

  @media (prefers-color-scheme: dark) {
    @keyframes track {
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
  }
</style>
