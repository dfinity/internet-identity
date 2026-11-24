<!--
  The texture a layout lays under its brand area: one of textures.ts, or the
  product's own FlairCanvas set as today's landing page sets it. Fills the
  box it's put in (which must be positioned), behind what the box holds.
-->
<script lang="ts">
  import FlairCanvas from "$lib/components/backgrounds/FlairCanvas.svelte";
  import type { ClassValue } from "svelte/elements";
  import type { TextureId } from "../playground.svelte";
  import Texture from "./Texture.svelte";

  interface Props {
    texture: TextureId;
    /** A colour for the marks; by default the class's text colour. */
    ink?: string;
    ambient?: boolean;
    class?: ClassValue;
  }

  const { texture, ink, ambient = false, class: className }: Props = $props();
</script>

{#if texture === "flair"}
  <!-- FlairCanvas paints itself at z -50 inside its own stacking context. -->
  <div
    aria-hidden="true"
    class={["pointer-events-none absolute inset-0 isolate", className]}
  >
    {#key ink}
      <FlairCanvas
        spacing="small"
        aspect="wide"
        dotSize="medium"
        vignette="none"
        visibility="always"
        enableRandomPointSize
        enableRandomOpacity={false}
        pointSizeNoiseScale="medium"
        pointSizeNoiseMultiplier="medium"
        customColor={ink}
        customColorMode={ink === undefined ? undefined : "all"}
        springOrTween={{
          type: "spring",
          stiffness: "high",
          dampening: "medium",
        }}
      />
    {/key}
  </div>
{:else if texture !== "none"}
  {#key texture}
    <Texture kind={texture} {ink} {ambient} class={className} />
  {/key}
{/if}
