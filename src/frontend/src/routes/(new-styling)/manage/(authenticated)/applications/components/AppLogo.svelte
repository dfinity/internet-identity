<script lang="ts">
  import { GlobeIcon } from "@lucide/svelte";

  interface Props {
    /** A ready `<img src>`, or `undefined` for an app that publishes none. */
    logo?: string;
    size: "card" | "lg" | "tile";
  }

  const { logo, size }: Props = $props();

  // A logo that fails to decode falls back to the globe instead of a broken image;
  // keyed by value so a later, valid logo still renders.
  let failedLogo = $state<string>();
  const shown = $derived(logo !== failedLogo ? logo : undefined);

  // A tile fills edge to edge with a logo drawn as a full square, like an app
  // icon. A mark, a circle or a wordmark would run into the frame, so it keeps
  // a margin of the plate. Keyed by value, like the failed logo.
  let squareLogo = $state<string>();
  const fillsTile = $derived(
    size === "tile" && shown !== undefined && shown === squareLogo,
  );

  // Square, with most of every edge opaque: a rounded square passes, a circle
  // doesn't. A logo from another origin can't be read and keeps the margin.
  const isSquareArt = (img: Element): boolean => {
    if (
      !(img instanceof HTMLImageElement) ||
      img.naturalWidth === 0 ||
      Math.abs(img.naturalWidth / img.naturalHeight - 1) > 0.05
    ) {
      return false;
    }
    const n = 32;
    const canvas = document.createElement("canvas");
    canvas.width = n;
    canvas.height = n;
    const context = canvas.getContext("2d", { willReadFrequently: true });
    if (context === null) {
      return false;
    }
    context.drawImage(img, 0, 0, n, n);
    let data: Uint8ClampedArray;
    try {
      data = context.getImageData(0, 0, n, n).data;
    } catch {
      return false;
    }
    // Opaque pixels along each edge, one pixel in, past the antialiasing.
    const counts = [0, 0, 0, 0];
    const alpha = (x: number, y: number) => data[(y * n + x) * 4 + 3];
    for (let t = 0; t < n; t++) {
      [alpha(t, 1), alpha(t, n - 2), alpha(1, t), alpha(n - 2, t)].forEach(
        (value, edge) => (counts[edge] += value >= 200 ? 1 : 0),
      );
    }
    return counts.every((count) => count >= n * 0.6);
  };
</script>

<!-- Decorative: the app's name sits right beside it. -->
<span
  aria-hidden="true"
  class={[
    "flex shrink-0 items-center justify-center overflow-hidden",
    // Only the globe needs a frame; a logo brings its own.
    shown === undefined &&
      size !== "tile" &&
      "border-border-tertiary bg-bg-primary text-fg-tertiary border",
    // In a grid of tiles every logo sits on the same light plate, in both
    // themes: most logos are drawn for a light page and vanish on a dark one.
    // A ring, not a border, so the plate keeps its size. A logo that redraws
    // itself for dark mode is told the plate is light.
    size === "tile" &&
      "text-surface-light-600 bg-white [color-scheme:light] shadow-[0_0_0_1px_rgb(0_0_0/0.06),0_1px_2px_rgb(0_0_0/0.06)] dark:shadow-[0_0_0_1px_rgb(255_255_255/0.08),0_1px_2px_rgb(0_0_0/0.4)]",
    {
      card: "size-11 rounded-lg",
      lg: "size-12 rounded-xl",
      tile: ["size-16 rounded-2xl", !fillsTile && "p-2.5"],
    }[size],
  ]}
>
  {#if shown !== undefined}
    <img
      src={shown}
      alt=""
      class={["size-full", fillsTile ? "object-cover" : "object-contain"]}
      onload={(event) => {
        if (size === "tile") {
          squareLogo = isSquareArt(event.currentTarget) ? shown : undefined;
        }
      }}
      onerror={() => (failedLogo = shown)}
    />
  {:else}
    <GlobeIcon class={{ card: "size-5", lg: "size-6", tile: "size-7" }[size]} />
  {/if}
</span>
