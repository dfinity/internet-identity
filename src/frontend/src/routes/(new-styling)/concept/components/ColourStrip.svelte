<script lang="ts" module>
  import { IDENTITY_PALETTES } from "$lib/utils/identityColors";

  /** The colours an identity can take, by their place in IDENTITY_PALETTES:
   *  ten of the 21 pairs, once around the hue wheel, light grounds and
   *  saturated ones mixed, so a row of them reads as ten different colours.
   *  They hold the three the mock identities' numbers give. */
  export const COLOUR_CHOICES: readonly number[] = [
    "Golden Hour",
    "Bite Your Tongue",
    "Diamond Day",
    "Late Gift",
    "Milo's Big Day",
    "Robed Witness",
    "Brain Waves",
    "Mr. Business",
    "Block Party",
    "Hidden Garden",
  ].map((name) => IDENTITY_PALETTES.findIndex((pair) => pair.name === name));
</script>

<script lang="ts">
  import { untrack } from "svelte";
  import { UserIcon } from "@lucide/svelte";
  import { MediaQuery } from "svelte/reactivity";
  import Popup from "./Popup.svelte";

  interface Props {
    anchor: HTMLElement;
    /** The identity's initial, so each swatch is the avatar it would make. */
    initial?: string;
    /** Where the identity's colours stand in IDENTITY_PALETTES now. */
    current: number;
    onPick: (paletteIndex: number) => void;
    onClose: () => void;
  }

  const { anchor, initial, current, onPick, onClose }: Props = $props();

  // A colour the number gave that isn't among the choices still shows, first,
  // so the strip always holds the one in use.
  const choices = $derived(
    COLOUR_CHOICES.includes(current)
      ? COLOUR_CHOICES
      : [current, ...COLOUR_CHOICES],
  );
  /** One row where there's room for it, two on a phone. */
  const wide = new MediaQuery("min-width: 480px");

  let swatches = $state<HTMLButtonElement[]>([]);

  // Focus starts on the colour in use, so arrow keys move from there.
  $effect(() => {
    const index = choices.indexOf(untrack(() => current));
    requestAnimationFrame(() => swatches[index]?.focus({ preventScroll: true }));
  });

  // Arrow keys walk the row and pick as they go, as a radio group does.
  const moveFrom = (event: KeyboardEvent, index: number) => {
    const rtl =
      getComputedStyle(event.currentTarget as Element).direction === "rtl";
    const last = choices.length - 1;
    const next =
      event.key === "Home"
        ? 0
        : event.key === "End"
          ? last
          : event.key === "ArrowLeft" || event.key === "ArrowUp"
            ? index + (rtl && event.key === "ArrowLeft" ? 1 : -1)
            : event.key === "ArrowRight" || event.key === "ArrowDown"
              ? index + (rtl && event.key === "ArrowRight" ? -1 : 1)
              : undefined;
    if (next === undefined || next < 0 || next > last) {
      return;
    }
    event.preventDefault();
    onPick(choices[next]);
    swatches[next]?.focus();
  };

  // Escape puts the row away wherever focus is, as a click elsewhere does.
  const onWindowKeydown = (event: KeyboardEvent) => {
    if (event.key === "Escape" && !event.defaultPrevented) {
      event.preventDefault();
      onClose();
    }
  };

  /** The hairline IdentityAvatar draws, so a pale pair stays on a pale page. */
  const HAIRLINE =
    "outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12";
</script>

<svelte:window onkeydown={onWindowKeydown} />

<!-- The colour, picked on the spot: one quiet row of the avatars it would
     make, the one in use ringed. A pick applies at once, so there's nothing
     to save; Escape or a click elsewhere puts the row away. -->
<Popup
  {anchor}
  {onClose}
  direction="down"
  align="start"
  distance="0.5rem"
  responsive={false}
  class="!w-max !rounded-[1.25rem]"
>
  <div
    role="radiogroup"
    aria-label="Colour"
    class={[
      "grid gap-1 p-1.5",
      wide.current ? "grid-flow-col" : "grid-cols-5",
    ]}
  >
    {#each choices as paletteIndex, index (paletteIndex)}
      {@const pair = IDENTITY_PALETTES[paletteIndex]}
      {@const checked = paletteIndex === current}
      <button
        bind:this={swatches[index]}
        type="button"
        role="radio"
        aria-checked={checked}
        aria-label={pair.name}
        title={pair.name}
        tabindex={checked ? 0 : -1}
        onclick={() => onPick(paletteIndex)}
        onkeydown={(event) => moveFrom(event, index)}
        class={[
          "grid size-7 place-items-center rounded-full text-xs font-semibold",
          "ring-offset-2 ring-offset-(color:--ii-popup-surface)",
          "focus-visible:ring-focus-ring outline-none focus-visible:ring-2",
          "transition-[scale,box-shadow] duration-160 ease-out active:scale-[0.94] motion-reduce:transition-none motion-reduce:active:scale-100",
          HAIRLINE,
          checked && "ring-fg-primary ring-2",
        ]}
        style:background-color={pair.background}
        style:color={pair.foreground}
      >
        {#if initial !== undefined}
          <span aria-hidden="true">{initial}</span>
        {:else}
          <UserIcon class="size-4" aria-hidden="true" />
        {/if}
      </button>
    {/each}
  </div>
</Popup>
