<!--
  One layout in the overview: the page itself, in a frame laid out at
  1440×900 and scaled down to the card's width, so it shows as a laptop
  would. It takes no pointer: the card around it is the button.
-->
<script lang="ts">
  interface Props {
    src: string;
    layout: string;
    title: string;
    frame?: HTMLIFrameElement;
    onload?: () => void;
  }

  let { src, layout, title, frame = $bindable(), onload }: Props = $props();

  const WIDTH = 1440;
  const HEIGHT = 900;
  let width = $state(0);
  const scale = $derived(width / WIDTH);
</script>

<span
  bind:clientWidth={width}
  class="relative block aspect-[16/10] w-full overflow-hidden rounded-2xl shadow-(--ii-shadow-4) transition-[scale,box-shadow] duration-220 ease-(--ii-ease-out) group-hover:scale-[1.01] group-hover:shadow-(--ii-shadow-6) motion-reduce:transition-none"
>
  {#if width > 0}
    <iframe
      bind:this={frame}
      {src}
      {title}
      {onload}
      data-layout={layout}
      loading="lazy"
      tabindex="-1"
      class="pointer-events-none absolute top-0 left-0 origin-top-left"
      style:width="{WIDTH}px"
      style:height="{HEIGHT}px"
      style:transform="scale({scale})"
    ></iframe>
  {/if}
</span>
