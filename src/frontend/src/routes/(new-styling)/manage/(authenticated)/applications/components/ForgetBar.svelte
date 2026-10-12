<script lang="ts">
  import { plural, t } from "$lib/stores/locale.store";

  interface Props {
    count: number;
    onCancel: () => void;
    onForget: () => void;
  }

  const { count, onCancel, onForget }: Props = $props();
</script>

<!-- Starts where the sidebar ends, so the bar lines up with the page's content. -->
<div
  class="from-bg-primary_alt pointer-events-none fixed inset-x-0 bottom-0 z-25 flex justify-start bg-linear-to-t from-40% to-transparent px-4 pt-12 pb-4 sm:start-[calc(--spacing(19)+1px)] sm:px-8 md:start-[calc(--spacing(74)+1px)] md:px-12 md:pt-16 md:pb-6"
>
  <div class="flex w-full max-w-3xl justify-center">
    <div
      role="toolbar"
      aria-label={$t`Forget apps`}
      class="rise border-border-tertiary bg-bg-primary_alt pointer-events-auto flex w-full max-w-100 items-center gap-2 rounded-xl border py-2 ps-4 pe-2 shadow-lg"
    >
      <span
        class="text-text-primary min-w-0 flex-1 truncate text-sm font-semibold"
      >
        {count === 0
          ? $t`No apps selected`
          : $plural(count, { one: "1 app selected", other: "# apps selected" })}
      </span>
      <button onclick={onCancel} class="btn btn-tertiary btn-sm">
        {$t`Cancel`}
      </button>
      <button
        onclick={onForget}
        disabled={count === 0}
        class="btn btn-primary btn-sm btn-danger"
      >
        {$t`Forget`}
      </button>
    </div>
  </div>
</div>

<style>
  @media (prefers-reduced-motion: no-preference) {
    .rise {
      animation: rise 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    }
  }

  @keyframes rise {
    from {
      opacity: 0;
      transform: translateY(16px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }
</style>
