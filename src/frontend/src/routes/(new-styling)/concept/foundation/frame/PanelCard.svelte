<script lang="ts">
  import type { Snippet } from "svelte";
  import { XIcon } from "@lucide/svelte";
  import { MediaQuery } from "svelte/reactivity";
  import { TALL_PANEL } from "./panel";
  import Elevated from "../surfaces/Elevated.svelte";

  interface Props {
    title: string;
    /** For Close: "Close OpenChat". */
    closeLabel: string;
    onClose: () => void;
    /** The card's glyph: an app's logo, or a section's icon on its tile. It
     *  gets the size to draw at, 44px where the panel is tall, else 36. */
    icon: Snippet<[large: boolean]>;
    /** What this is, under the title, in the section cards' quiet line. */
    lead?: Snippet;
    children: Snippet;
  }

  const { title, closeLabel, onClose, icon, lead, children }: Props = $props();

  const tall = new MediaQuery(TALL_PANEL);
  const large = $derived(tall.current);
  const titleId = $props.id();
</script>

<!-- One thing the panel shows in place of its overview, as an open section
     does (Thomas: the accordion "feels less like navigating"): the same
     head, its glyph and title, Close where a section's is, and what it holds
     under them. Large or compact as the section cards are. -->
<Elevated
  offset={1}
  as="section"
  aria-labelledby={titleId}
  class="flex flex-col overflow-hidden rounded-2xl"
>
  <div
    class={[
      "relative px-4",
      large
        ? "grid grid-cols-[auto_minmax(0,1fr)_auto] items-center pt-5"
        : "flex items-center gap-3 pt-3.5",
    ]}
  >
    {@render icon(large)}
    <h2
      id={titleId}
      class={[
        "text-text-primary min-w-0 truncate font-semibold",
        large
          ? "col-span-full row-start-2 mt-3.5 text-[1.0625rem] leading-6 tracking-[-0.01em]"
          : "flex-1 text-sm",
      ]}
    >
      {title}
    </h2>
    <span
      class={[
        "-me-2.5 grid size-9 shrink-0 place-items-center",
        large && "col-start-3 row-start-1",
      ]}
    >
      <!-- Large, at the icon's top by its ink, as a section card's Close. -->
      <button
        type="button"
        onclick={onClose}
        class={[
          "btn btn-tertiary btn-icon btn-sm rounded-full",
          large && "relative -top-[17px]",
        ]}
      >
        <XIcon class="size-4.5" aria-hidden="true" />
        <span>{closeLabel}</span>
      </button>
    </span>
  </div>
  {#if lead !== undefined}
    <div
      class={[
        "text-text-tertiary px-4 text-[13px] leading-[18px]",
        large ? "pt-1" : "pt-2.5",
      ]}
    >
      {@render lead()}
    </div>
  {/if}
  {@render children()}
</Elevated>
