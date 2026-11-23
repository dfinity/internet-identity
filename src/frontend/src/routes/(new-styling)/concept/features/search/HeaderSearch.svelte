<script lang="ts">
  /**
   * Finding an app from any page: a search glyph in the header, so search
   * isn't a part of Explore only. It opens the page's one search
   * (SearchOverlay), which ⌘K opens on desktop too.
   */
  import { SearchIcon } from "@lucide/svelte";
  import type { ClassValue } from "svelte/elements";

  interface Props {
    /** Opens the search; called within the press, for a phone's keyboard. */
    onclick: () => void;
    /** A phone: a bare glyph in the header's pill. */
    compact?: boolean;
    /** Content runs under the header: the glyph's pill lifts off it, as the
     *  header's other pills do. */
    lifted?: boolean;
    class?: ClassValue;
  }

  const {
    onclick,
    compact = false,
    lifted = false,
    class: className,
  }: Props = $props();
</script>

<button
  type="button"
  {onclick}
  aria-label="Search apps"
  aria-haspopup="dialog"
  aria-keyshortcuts={compact ? undefined : "Meta+K Control+K"}
  title={compact ? undefined : "Search apps  ⌘K"}
  class={[
    "focus-visible:outline-focus-ring grid shrink-0 place-items-center rounded-full transition-[background-color,box-shadow,color] duration-200 ease-out focus-visible:outline-2 motion-reduce:transition-none",
    compact
      ? "text-fg-secondary size-8 hover:bg-(--ii-hover) active:bg-(--ii-selected)"
      : [
          "ii-hover text-fg-secondary hover:text-fg-primary pointer-events-auto size-10 border border-transparent focus-visible:outline-offset-2",
          lifted ? "ii-elevation-6 bg-(--ii-floating)" : "bg-(--ii-shelf)",
        ],
    className,
  ]}
>
  <SearchIcon
    class={compact ? "size-4" : "size-[1.125rem]"}
    aria-hidden="true"
  />
</button>
