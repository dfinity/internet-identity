<span class="relative inline-block aspect-square h-[1em] align-middle">
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 24 24"
    fill="currentColor"
    class="pulsating"
  >
    <circle cx="12" cy="12" r="10" />
  </svg>
</span>

<style>
  .pulsating {
    animation: pulsate 2s infinite;
    fill: var(--color-success-500);
  }

  @keyframes pulsate {
    0% {
      fill-opacity: 1;
    }
    50% {
      fill-opacity: 0.5;
    }
    100% {
      fill-opacity: 1;
    }
  }
</style>
