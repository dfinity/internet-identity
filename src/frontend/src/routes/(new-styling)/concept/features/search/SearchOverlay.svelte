<script lang="ts">
  /**
   * The one search over the page, for finding an app and adding one: the
   * header's glyph opens it, ⌘K on desktop, and so does Add app after the
   * last tile. What it lists is SearchPalette's.
   *
   * On desktop it opens a palette over the page, after beUI's Command
   * Palette (https://beui.dev/components/blocks/command-palette, MIT): the
   * page blurs behind it, and the panel stands a little above the middle,
   * so it grows down as it lists. It comes in from a step smaller and a
   * little higher, and leaves faster than it came. On a phone it opens over
   * the whole screen, the field where the header was and the keyboard up at
   * once, as a phone's search does; Cancel puts it away.
   */
  import { flushSync } from "svelte";
  import { SearchIcon, XIcon } from "@lucide/svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import type {
    ConceptApp,
    ExploreShelfItem,
    FeaturedApp,
  } from "../../mock/mockData";
  import SearchPalette from "./SearchPalette.svelte";
  import { FLOATING } from "../../foundation/surfaces/Elevated.svelte";
  import SurfaceLevel from "../../foundation/surfaces/SurfaceLevel.svelte";

  interface Props {
    apps: ConceptApp[];
    isAdded: (origin: string) => boolean;
    onAdd: (app: FeaturedApp) => void;
    onAddLink: (host: string) => void;
    /** An app was opened from the search. */
    onOpen: (app: FeaturedApp) => void;
    /** Featured apps the identity doesn't use yet. */
    suggested: FeaturedApp[];
    /** A phone: the search opens over the whole screen. */
    compact?: boolean;
  }

  const {
    apps,
    isAdded,
    onAdd,
    onAddLink,
    onOpen,
    suggested,
    compact = false,
  }: Props = $props();

  const PLACEHOLDER = "Search apps or paste a link";
  const reduced = $derived(prefersReducedMotion.current);
  /** Emil Kowalski's strong ease-out, as --ii-ease-out. */
  const EASE_OUT = "cubic-bezier(0.23, 1, 0.32, 1)";

  let query = $state("");
  let shelf = $state<ExploreShelfItem>();
  let open = $state(false);
  let closing = false;
  let dialog = $state<HTMLDialogElement>();
  let input = $state<HTMLInputElement>();
  let scrim = $state<HTMLElement>();
  let panel = $state<HTMLElement>();
  let palette = $state<ReturnType<typeof SearchPalette>>();
  const uid = $props.id();
  const listId = `${uid}-list`;
  let activeOptionId = $state<string>();

  /** Opens the search. Call it within the press itself, so a phone brings
   *  its keyboard up with it: a focus from a timer or after a frame would
   *  leave it down. */
  export const show = () => {
    if (open) return;
    query = "";
    shelf = undefined;
    open = true;
    flushSync();
    dialog?.showModal();
    input?.focus();
    if (reduced) return;
    if (compact) {
      dialog?.animate([{ opacity: 0 }, { opacity: 1 }], {
        duration: 160,
        easing: "ease-out",
      });
      return;
    }
    scrim?.animate([{ opacity: 0 }, { opacity: 1 }], {
      duration: 180,
      easing: EASE_OUT,
    });
    panel?.animate(
      [
        { opacity: 0, transform: "translateY(-8px) scale(0.97)" },
        { opacity: 1, transform: "none" },
      ],
      { duration: 260, easing: EASE_OUT },
    );
  };

  const hide = async () => {
    if (dialog === undefined || !dialog.open || closing) return;
    closing = true;
    input?.blur();
    if (!reduced) {
      const leaving = compact
        ? [
            dialog.animate([{ opacity: 1 }, { opacity: 0 }], {
              duration: 120,
              easing: "ease-in",
              fill: "forwards",
            }),
          ]
        : [
            scrim?.animate([{ opacity: 1 }, { opacity: 0 }], {
              duration: 120,
              easing: EASE_OUT,
              fill: "forwards",
            }),
            panel?.animate(
              [
                { opacity: 1, transform: "none" },
                { opacity: 0, transform: "translateY(-8px) scale(0.97)" },
              ],
              { duration: 120, easing: EASE_OUT, fill: "forwards" },
            ),
          ];
      await Promise.all(leaving.map((animation) => animation?.finished));
    }
    dialog.close();
    open = false;
    closing = false;
  };

  /** Escape takes back one step at a time: the shelf, then what's typed,
   *  then the search itself. */
  const back = () => {
    if (shelf !== undefined) {
      shelf = undefined;
    } else if (query !== "") {
      query = "";
    } else {
      void hide();
    }
  };

  const onKeydown = (event: KeyboardEvent) => {
    if (event.isComposing) return;
    if (event.key === "Escape") {
      event.preventDefault();
      back();
    } else if (
      event.key === "Backspace" &&
      query === "" &&
      shelf !== undefined
    ) {
      event.preventDefault();
      shelf = undefined;
    } else if (palette?.keydown(event) === true) {
      event.preventDefault();
    }
  };

  const pickShelf = (one: ExploreShelfItem) => {
    shelf = one;
    query = "";
    input?.focus();
  };
  const openApp = (app: FeaturedApp) => {
    onOpen(app);
    void hide();
  };

  // ⌘K (Ctrl+K elsewhere) opens it from any page, and closes it again.
  const onWindowKeydown = (event: KeyboardEvent) => {
    if (
      !compact &&
      (event.metaKey || event.ctrlKey) &&
      event.key.toLowerCase() === "k"
    ) {
      event.preventDefault();
      if (open) {
        void hide();
      } else {
        show();
      }
    }
  };
</script>

<svelte:window onkeydown={onWindowKeydown} />

{#snippet field(big: boolean)}
  <!-- A shelf picked from the chips stands at the field's start as a token,
       and Backspace in an empty field takes it out. -->
  <SearchIcon
    class={["text-fg-tertiary shrink-0", big ? "size-5" : "size-4"]}
    aria-hidden="true"
  />
  {#if shelf !== undefined}
    <span
      class="text-text-primary flex h-7 shrink-0 items-center gap-0.5 rounded-full bg-(--ii-selected) ps-3 pe-1 text-sm font-medium"
    >
      {shelf.title}
      <button
        type="button"
        onclick={() => {
          shelf = undefined;
          input?.focus();
        }}
        aria-label={`Show all apps, not only ${shelf.title}`}
        class="text-fg-tertiary hover:text-fg-secondary grid size-5 place-items-center rounded-full"
      >
        <XIcon class="size-3.5" aria-hidden="true" />
      </button>
    </span>
  {/if}
  <input
    bind:this={input}
    bind:value={query}
    onkeydown={onKeydown}
    type="search"
    role="combobox"
    aria-expanded="true"
    aria-controls={listId}
    aria-activedescendant={activeOptionId}
    aria-autocomplete="list"
    placeholder={shelf !== undefined ? `Search ${shelf.title}` : PLACEHOLDER}
    aria-label={PLACEHOLDER}
    autocomplete="off"
    autocapitalize="off"
    spellcheck="false"
    enterkeyhint="search"
    class={[
      "text-text-primary placeholder:text-text-placeholder h-full min-w-0 flex-1 appearance-none border-0 bg-transparent p-0 outline-none focus:ring-0 [&::-webkit-search-cancel-button]:appearance-none",
      // 16px everywhere a keyboard comes up, so iOS doesn't zoom in.
      big ? "text-[17px]" : "text-base",
    ]}
  />
{/snippet}

{#snippet body(footer: boolean)}
  <SearchPalette
    bind:this={palette}
    {listId}
    onActive={(id) => (activeOptionId = id)}
    {query}
    {shelf}
    onShelf={pickShelf}
    {apps}
    {isAdded}
    {onAdd}
    {onAddLink}
    onOpen={openApp}
    {suggested}
    {footer}
  />
{/snippet}

{#if compact}
  <!-- Phone: over the whole screen, in the top layer, over the tab bar. -->
  <dialog
    bind:this={dialog}
    aria-label="Search apps"
    oncancel={(event) => {
      event.preventDefault();
      back();
    }}
    class="text-text-primary pointer-events-auto fixed inset-0 m-0 h-dvh max-h-none w-full max-w-none flex-col bg-(--ii-canvas) p-0 backdrop:bg-transparent open:flex"
  >
    {#if open}
      <div class="h-[env(safe-area-inset-top)] shrink-0"></div>
      <!-- Where the header's row was, as tall, on the same edge. -->
      <div class="flex shrink-0 items-center gap-1 px-(--ii-edge) py-3">
        <div
          class="flex h-10 min-w-0 flex-1 items-center gap-2 rounded-full bg-(--ii-shelf) ps-3.5 pe-3"
        >
          {@render field(false)}
        </div>
        <button
          type="button"
          onclick={() => void hide()}
          class="text-text-secondary hover:text-text-primary focus-visible:outline-focus-ring -me-2 h-10 shrink-0 rounded-full px-2 text-base font-medium transition-colors duration-150 focus-visible:outline-2"
        >
          Cancel
        </button>
      </div>
      <div
        class="flex min-h-0 flex-1 flex-col px-2 pb-[env(safe-area-inset-bottom)]"
      >
        {@render body(false)}
      </div>
    {/if}
  </dialog>
{:else}
  <!-- Desktop: the palette, in the top layer over everything. It takes the
       pointer whatever it's rendered in (pointer-events inherits even into
       the top layer): a press anywhere on the blur closes it. The dialog
       itself is a bare frame; the blur behind and the panel are its own, so
       each comes and goes on its own timing. -->
  <dialog
    bind:this={dialog}
    aria-label="Search apps"
    oncancel={(event) => {
      event.preventDefault();
      back();
    }}
    class="pointer-events-auto fixed inset-0 m-0 h-dvh max-h-none w-full max-w-none bg-transparent p-0 backdrop:bg-transparent"
  >
    {#if open}
      <button
        bind:this={scrim}
        type="button"
        tabindex="-1"
        aria-label="Close search"
        onclick={() => void hide()}
        class="absolute inset-0 cursor-default bg-black/5 [backdrop-filter:blur(12px)_saturate(140%)] [-webkit-backdrop-filter:blur(12px)_saturate(140%)] dark:bg-black/25"
      ></button>
      <!-- A little above the middle, so the panel grows down as it lists. -->
      <div
        class="pointer-events-none absolute inset-x-4 top-[18vh] bottom-4 flex items-start justify-center"
      >
        <!-- Floating over the page: five levels above it, as the surface
             ladder has it, with the corners its rows' 16px and its 8px
             padding make. -->
        <div
          bind:this={panel}
          class="ii-elevation-6 pointer-events-auto flex max-h-full w-full max-w-xl origin-top flex-col overflow-hidden rounded-3xl bg-(--ii-floating) will-change-transform"
        >
          <SurfaceLevel level={FLOATING}>
            <div
              class="border-border-tertiary flex h-14 shrink-0 items-center gap-3 border-b px-5"
            >
              {@render field(true)}
            </div>
            <div class="flex max-h-[min(34rem,60vh)] min-h-0 flex-col">
              {@render body(true)}
            </div>
          </SurfaceLevel>
        </div>
      </div>
    {/if}
  </dialog>
{/if}
