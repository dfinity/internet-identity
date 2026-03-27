<script lang="ts">
  import { normalizeProps, useMachine } from "@zag-js/svelte";
  import * as toast from "@zag-js/toast";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { onMount, untrack } from "svelte";

  interface ToastProps {
    toast: toast.Options;
    index: number;
    parent: toast.GroupService;
  }

  type OnlyLiterals<T> = T extends string
    ? string extends T
      ? never
      : T
    : never;

  const { toast: toastProps, index, parent }: ToastProps = $props();

  const machineProps = $derived({ ...toastProps, parent, index });
  const service = useMachine(toast.machine, () => machineProps);
  const api = $derived(toast.connect(service, normalizeProps));

  const variant = $derived(api.type as OnlyLiterals<typeof api.type>);
  const hasCountdown = untrack(
    () => toastProps.action !== undefined && toastProps.duration !== undefined,
  );
  let innerWidth = $state<number>(window.innerWidth);
  const mobile = $derived(innerWidth < 480);

  let progress = $state(100);

  onMount(() => {
    if (!hasCountdown) return;

    const duration = untrack(() => toastProps.duration!);
    let runningTime = 0;
    let lastTick = Date.now();
    let rafId: number;

    const tick = () => {
      const now = Date.now();
      if (!api.paused) {
        runningTime += now - lastTick;
      }
      lastTick = now;
      progress = Math.max(0, 100 - (runningTime / duration) * 100);
      if (progress > 0) {
        rafId = requestAnimationFrame(tick);
      }
    };
    rafId = requestAnimationFrame(tick);

    return () => cancelAnimationFrame(rafId);
  });
</script>

<svelte:window bind:innerWidth />

{#snippet countdownIcon()}
  <ProgressRing value={progress} class="text-fg-brand-primary" />
{/snippet}

<div {...api.getRootProps()}>
  <Alert
    title={api.title}
    description={api.description}
    {variant}
    onClose={api.closable ? api.dismiss : undefined}
    direction={mobile ? "vertical" : "horizontal"}
    icon={hasCountdown ? countdownIcon : undefined}
    class={[
      "shadow-lg not-dark:bg-clip-padding max-sm:w-[calc(100vw_-_2rem)] sm:w-100",
      "!border-black/[0.08]",
      "dark:!border-surface-dark-700",
    ]}
  >
    {#if toastProps.action}
      <button
        {...api.getActionTriggerProps()}
        class="text-text-primary text-sm font-semibold hover:underline"
      >
        {toastProps.action.label}
      </button>
    {/if}
  </Alert>
</div>

<style>
  [data-part="root"] {
    translate: var(--x) var(--y);
    scale: var(--scale);
    z-index: var(--z-index);
    height: var(--height);
    opacity: var(--opacity);
    will-change: translate, opacity, scale;
  }

  [data-part="root"] {
    transition:
      translate 400ms,
      scale 400ms,
      opacity 400ms;
    transition-timing-function: cubic-bezier(0.21, 1.02, 0.73, 1);
  }

  [data-part="root"][data-state="closed"] {
    transition:
      translate 400ms,
      scale 400ms,
      opacity 200ms;
    transition-timing-function: cubic-bezier(0.06, 0.71, 0.55, 1);
  }
</style>
