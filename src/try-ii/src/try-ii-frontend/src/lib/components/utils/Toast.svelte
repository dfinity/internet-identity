<script lang="ts">
  import { normalizeProps, useMachine } from "@zag-js/svelte";
  import * as toast from "@zag-js/toast";
  import Alert from "$lib/components/utils/Alert.svelte";

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
  let innerWidth = $state<number>(window.innerWidth);
  const mobile = $derived(innerWidth < 480);
</script>

<svelte:window bind:innerWidth />

<div {...api.getRootProps()}>
  <Alert
    title={api.title}
    description={api.description}
    {variant}
    onClose={api.closable ? api.dismiss : undefined}
    direction={mobile ? "vertical" : "horizontal"}
    class={[
      "shadow-lg not-dark:bg-clip-padding max-sm:w-[calc(100vw_-_2rem)] sm:w-100",
      "!border-black/[0.08]",
      "dark:!border-surface-dark-700",
    ]}
  />
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
