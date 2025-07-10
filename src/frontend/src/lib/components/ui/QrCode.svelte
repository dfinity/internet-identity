<script lang="ts" module>
  import type QrCreator from "qr-creator";
  import { isNullish } from "@dfinity/utils";

  // Lazy load qr creator only once
  let qrCreatorPromise: Promise<typeof QrCreator> | undefined;
  let getQrCreator = (): Promise<typeof QrCreator> => {
    if (isNullish(qrCreatorPromise)) {
      qrCreatorPromise = import("qr-creator").then((value) => value.default);
    }
    return qrCreatorPromise;
  };
</script>

<script lang="ts">
  import { onMount } from "svelte";
  import type { HTMLAttributes } from "svelte/elements";

  interface Props extends Omit<HTMLAttributes<HTMLDivElement>, "children"> {
    text: string;
  }

  const { text, class: className, ...props }: Props = $props();

  let containerRef = $state<HTMLDivElement>();
  let clientWidth = $state<number>(0);
  let clientHeight = $state<number>(0);
  let qrCreator = $state<typeof QrCreator>();

  $effect(() => {
    if (isNullish(qrCreator) || isNullish(containerRef)) {
      return;
    }
    const size = clientWidth > clientHeight ? clientWidth : clientHeight;
    const oldCanvas = containerRef.querySelector("canvas");
    qrCreator.render(
      {
        text,
        size: size * window.devicePixelRatio,
        fill: "#000000",
        background: "#ffffff",
        radius: 0,
      },
      containerRef,
    );
    oldCanvas?.remove();
  });

  // Lazy load qr creator on component mount
  onMount(() => {
    getQrCreator().then((value) => (qrCreator = value));
  });
</script>

<div
  bind:clientWidth
  bind:clientHeight
  class={[className, "relative container"]}
  {...props}
>
  <div
    bind:this={containerRef}
    class="mix-blend-darken dark:mix-blend-lighten **:dark:invert"
  ></div>
  <div
    class={"bg-text-primary absolute inset-0 mix-blend-lighten dark:mix-blend-darken"}
  ></div>
</div>

<style>
  .container :global(canvas) {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
  }
</style>
