<script lang="ts">
  /**
   * What the command line shows: in the palette over the page on desktop,
   * and over the whole screen on a phone. It is a command line first and a
   * search second (Misha, 23 Nov, after Linear's ⌘K): before a letter is
   * typed it offers what people come to do (what the account waits on, then
   * the usual steps) and the apps used last. From the first letter it lists
   * what matches: your apps, to open, then the commands, then the rest of
   * the catalogue, to add, and a pasted link as an app of its own.
   *
   * Turned to adding an app (the tile after the last app, or the command),
   * it is the search it was: Explore's shelves to browse, the apps used
   * last and a few to try. A shelf picked from the chips narrows the list
   * to it, and stands in the field as a token (SearchOverlay).
   *
   * Every row is one action: a command runs, Open for an app in My apps,
   * Add for the rest. One highlight marks the row the keyboard or the
   * pointer is on and glides from row to row, as in beUI's Command Palette
   * (https://beui.dev/components/blocks/command-palette, MIT). Arrows move
   * it, left and right along a row of chips or apps, and Enter does what
   * the row says.
   */
  import { untrack } from "svelte";
  import { LinkIcon } from "@lucide/svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import AppLogo from "../../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import { asLink, searchCatalog } from "../explore/catalog";
  import { matchesCommand, type Command } from "./commands";
  import {
    exploreFeed,
    type ConceptApp,
    type ExploreShelfItem,
    type FeaturedApp,
  } from "../../mock/mockData";

  interface Props {
    query: string;
    /** `all`: the command line. `apps`: finding an app to add. */
    mode: "all" | "apps";
    /** What the page can do, for the command line. */
    commands: Command[];
    onCommand: (command: Command) => void;
    /** Adding an app was reached from the command line: Escape steps back
     *  to it. */
    stepped?: boolean;
    /** The shelf the list is narrowed to, picked from the chips. */
    shelf?: ExploreShelfItem;
    onShelf: (shelf: ExploreShelfItem) => void;
    /** The identity's own apps. */
    apps: ConceptApp[];
    isAdded: (origin: string) => boolean;
    onAdd: (app: FeaturedApp) => void;
    onAddLink: (host: string) => void;
    /** An app was opened from the search (in a new tab). */
    onOpen: (app: FeaturedApp) => void;
    /** Featured apps the identity doesn't use yet. */
    suggested: FeaturedApp[];
    /** Desktop: a foot with the keys. */
    footer?: boolean;
    /** The list's id, for the field, which is the combobox. */
    listId: string;
    /** The option the highlight is on, for the field. */
    onActive?: (optionId: string | undefined) => void;
  }

  const {
    query,
    mode,
    commands,
    onCommand,
    stepped = false,
    shelf,
    onShelf,
    apps,
    isAdded,
    onAdd,
    onAddLink,
    onOpen,
    suggested,
    footer = false,
    listId,
    onActive,
  }: Props = $props();

  const uid = $props.id();
  const reduced = $derived(prefersReducedMotion.current);

  const SHELVES = exploreFeed.filter(
    (item): item is ExploreShelfItem => item.kind === "shelf",
  );

  type Entry =
    | { kind: "command"; id: string; command: Command }
    | { kind: "chip"; id: string; shelf: ExploreShelfItem }
    | { kind: "tile"; id: string; app: ConceptApp }
    | { kind: "app"; id: string; app: FeaturedApp }
    | { kind: "link"; id: string; app: FeaturedApp };
  interface Group {
    id: string;
    title?: string;
    layout: "chips" | "tiles" | "rows";
    entries: Entry[];
  }

  const typed = $derived(query.trim());
  const lower = $derived(typed.toLowerCase());
  const holds = (app: FeaturedApp) =>
    app.name.toLowerCase().includes(lower) ||
    app.oneLiner?.toLowerCase().includes(lower) === true;
  const asRow = (app: FeaturedApp): Entry => ({
    kind: "app",
    id: `app:${app.origin}`,
    app,
  });
  const asCommand = (command: Command): Entry => ({
    kind: "command",
    id: `command:${command.id}`,
    command,
  });
  /** How many commands a list shows: the ones people come for, not all. */
  const COMMANDS_SHOWN = 5;

  const groups = $derived.by((): Group[] => {
    // A shelf picked: its apps, narrowed by what's typed.
    if (shelf !== undefined) {
      const listed = shelf.apps.filter((app) => typed === "" || holds(app));
      return [
        {
          id: "shelf",
          title: `${listed.length} ${listed.length === 1 ? "app" : "apps"}`,
          layout: "rows",
          entries: listed.map(asRow),
        },
      ];
    }
    // Nothing typed: what the search offers on its own.
    if (typed === "") {
      const recent = apps
        .filter((app) => app.lastUsedMillis > 0)
        .sort((a, b) => b.lastUsedMillis - a.lastUsedMillis)
        .slice(0, 6);
      const used: Group = {
        id: "recent",
        title: "Recent",
        layout: "tiles",
        entries: recent.map((app) => ({
          kind: "tile",
          id: `recent:${app.origin}`,
          app,
        })),
      };
      // The command line: what to do first, then the apps used last.
      if (mode === "all") {
        return [
          {
            id: "suggested-commands",
            title: "Suggested",
            layout: "rows",
            entries: commands
              .filter(({ suggested }) => suggested === true)
              .slice(0, COMMANDS_SHOWN)
              .map(asCommand),
          } satisfies Group,
          used,
        ].filter(({ entries }) => entries.length > 0);
      }
      return [
        {
          id: "browse",
          layout: "chips",
          entries: SHELVES.map((one) => ({
            kind: "chip",
            id: `shelf:${one.title}`,
            shelf: one,
          })),
        },
        used,
        {
          id: "suggested",
          title: "Suggested",
          layout: "rows",
          entries: suggested.slice(0, 3).map(asRow),
        },
      ].filter(({ entries }) => entries.length > 0) as Group[];
    }
    // Typed: yours first, then the rest. A link to an app that isn't
    // listed is offered as an app of its own, at the top.
    const matches = searchCatalog(typed);
    const host = asLink(typed);
    const link =
      host === undefined ||
      matches.some(({ origin }) => origin === `https://${host}`)
        ? undefined
        : {
            origin: `https://${host}`,
            name: host,
            logo: "",
            url: `https://${host}`,
          };
    // Yours in the catalog's order, then any added as a link.
    const mine = [
      ...matches.filter(({ origin }) => isAdded(origin)),
      ...apps.filter(
        (app) =>
          holds(app) && !matches.some(({ origin }) => origin === app.origin),
      ),
    ];
    const rest = matches.filter(({ origin }) => !isAdded(origin)).slice(0, 12);
    const yours: Group = {
      id: "mine",
      title: "My apps",
      layout: "rows",
      entries: mine.map(asRow),
    };
    const actions: Group = {
      id: "commands",
      title: "Actions",
      layout: "rows",
      entries:
        mode === "all"
          ? commands
              .filter((command) => matchesCommand(command, typed))
              .slice(0, COMMANDS_SHOWN)
              .map(asCommand)
          : [],
    };
    // An app of yours named as typed comes first, to open. One found only
    // by its description gives way to a command, which is found by the
    // start of its words.
    const named = mine.some(({ name }) => name.toLowerCase().includes(lower));
    return [
      {
        id: "link",
        layout: "rows",
        entries:
          link === undefined
            ? []
            : [{ kind: "link", id: `link:${host}`, app: link }],
      },
      ...(named ? [yours, actions] : [actions, yours]),
      {
        id: "rest",
        title: mine.length > 0 ? "More apps" : "Apps",
        layout: "rows",
        entries: rest.map(asRow),
      },
    ].filter(({ entries }) => entries.length > 0) as Group[];
  });

  /** What the keys move over: a row of chips or tiles is one line, read
   *  left to right; every other entry is a line of its own. */
  const lines = $derived(
    groups.flatMap((group) =>
      group.layout === "rows"
        ? group.entries.map((entry) => [entry])
        : [group.entries],
    ),
  );
  const flat = $derived(lines.flat());
  const optionId = (entry: Entry) => `${uid}-opt-${flat.indexOf(entry)}`;

  /** The entry the highlight is on, by id, so it stays on an app that moves
   *  (one just added goes from the rest to yours). */
  let activeId = $state<string>();
  const active = $derived(flat.find(({ id }) => id === activeId));
  $effect(() => {
    onActive?.(active === undefined ? undefined : optionId(active));
  });

  // A new search starts on its first match, so Enter takes it; the list
  // offered before a letter is typed waits for a key or the pointer.
  $effect(() => {
    void typed;
    void shelf;
    void mode;
    untrack(() => {
      activeId = typed !== "" || shelf !== undefined ? flat[0]?.id : undefined;
    });
  });

  const added = (entry: Entry) =>
    entry.kind === "tile" ||
    (entry.kind !== "chip" &&
      entry.kind !== "command" &&
      isAdded(entry.app.origin));
  /** What Enter (or a click) does on the entry, as its button says. */
  const verbOf = (entry: Entry) =>
    entry.kind === "command"
      ? "Run"
      : entry.kind === "chip"
        ? "Show"
        : added(entry)
          ? "Open"
          : "Add";

  const open = (app: FeaturedApp) => {
    window.open(app.url, "_blank", "noopener,noreferrer");
    onOpen(app);
  };
  const run = (entry: Entry) => {
    if (entry.kind === "command") {
      onCommand(entry.command);
      return;
    }
    if (entry.kind === "chip") {
      onShelf(entry.shelf);
      return;
    }
    const app = entry.app;
    if (added(entry)) {
      open(app);
    } else if (entry.kind === "link") {
      onAddLink(app.name);
    } else {
      onAdd(app);
    }
  };

  /** Moved by a key: the entry scrolls into view. The pointer only points. */
  let byKey = false;
  const moveTo = (line: number, column: number) => {
    const row = lines[line];
    if (row === undefined) return;
    byKey = true;
    activeId = row[Math.min(column, row.length - 1)].id;
  };
  const where = () => {
    for (let line = 0; line < lines.length; line++) {
      const column = lines[line].findIndex(({ id }) => id === activeId);
      if (column !== -1) return { line, column };
    }
    return undefined;
  };

  /** The field's keys, handed down; true when the list took one. */
  export const keydown = (event: KeyboardEvent): boolean => {
    const at = where();
    switch (event.key) {
      case "ArrowDown":
        moveTo(
          at === undefined ? 0 : Math.min(at.line + 1, lines.length - 1),
          at?.column ?? 0,
        );
        return true;
      case "ArrowUp":
        if (at === undefined) return true;
        if (at.line === 0) {
          activeId = undefined;
        } else {
          moveTo(at.line - 1, at.column);
        }
        return true;
      case "ArrowLeft":
      case "ArrowRight": {
        if (at === undefined || lines[at.line].length < 2) return false;
        const step =
          (event.key === "ArrowRight" ? 1 : -1) *
          (document.documentElement.dir === "rtl" ? -1 : 1);
        moveTo(
          at.line,
          Math.max(0, Math.min(at.column + step, lines[at.line].length - 1)),
        );
        return true;
      }
      case "Enter": {
        const entry = active ?? (typed !== "" ? flat[0] : undefined);
        if (entry === undefined) return false;
        run(entry);
        return true;
      }
    }
    return false;
  };

  // --- The highlight ---

  let content = $state<HTMLElement>();
  let mark = $state<{
    x: number;
    y: number;
    w: number;
    h: number;
    radius: string;
    glide: boolean;
  }>();
  $effect(() => {
    void groups;
    const id = activeId;
    const previous = untrack(() => mark);
    const element =
      id === undefined
        ? null
        : (content?.querySelector<HTMLElement>(
            `[data-entry="${CSS.escape(id)}"]`,
          ) ?? null);
    if (element === null || content === undefined) {
      mark = undefined;
      return;
    }
    // Into view first, then measured where it ended up.
    if (byKey) {
      element.scrollIntoView({ block: "nearest", inline: "nearest" });
      byKey = false;
    }
    const box = element.getBoundingClientRect();
    const frame = content.getBoundingClientRect();
    mark = {
      x: box.left - frame.left,
      y: box.top - frame.top,
      w: box.width,
      h: box.height,
      radius: element.dataset.shape === "pill" ? "9999px" : "1rem",
      glide: previous !== undefined,
    };
  });

  const point = (entry: Entry) => () => {
    if (activeId !== entry.id) activeId = entry.id;
  };
</script>

{#snippet verbPill(entry: Entry)}
  <span
    aria-hidden="true"
    class={[
      "btn btn-sm pointer-events-none min-w-16 shrink-0 rounded-full font-medium transition-colors duration-150",
      added(entry) ? "btn-secondary" : "btn-primary",
    ]}
  >
    {verbOf(entry)}
  </span>
{/snippet}

{#snippet entryView(entry: Entry)}
  {@const common = {
    id: optionId(entry),
    role: "option" as const,
    "aria-selected": entry.id === activeId,
    tabindex: -1,
    "data-entry": entry.id,
    onpointermove: point(entry),
    onclick: () => run(entry),
  }}
  {#if entry.kind === "command"}
    {@const command = entry.command}
    <!-- A command: its glyph on a plate at the row's start, in a slot as
         wide as an app's logo, so both kinds of row start their words on
         one line; what it leads to at the row's end. -->
    <button
      {...common}
      type="button"
      class="relative flex w-full items-center gap-3 rounded-2xl px-2 py-1.5 text-start"
    >
      <span aria-hidden="true" class="flex h-10 w-12 shrink-0 items-center">
        {#if "identity" in command.icon}
          <span class="grid size-10 place-items-center">
            <IdentityAvatar
              identity={command.icon.identity}
              palette={command.icon.palette}
              size="sm"
            />
          </span>
        {:else}
          {@const Icon = command.icon}
          <span
            class="text-fg-secondary relative grid size-10 place-items-center rounded-xl bg-(--ii-hover)"
          >
            <Icon class="size-4.5" />
            {#if command.dot !== undefined}
              <!-- The account waits on it: the section's own dot. -->
              <span
                class={[
                  "absolute -end-0.5 -top-0.5 size-2 rounded-full",
                  command.dot === "risk"
                    ? "bg-bg-error-solid"
                    : command.dot === "action"
                      ? "bg-bg-warning-solid"
                      : "bg-fg-primary",
                ]}
              ></span>
            {/if}
          </span>
        {/if}
      </span>
      <!-- Where the row is narrow (a phone), the hint alone gives way: the
           label keeps its words, up to the row's own room. -->
      <span
        class="text-text-primary max-w-[calc(100%-3.75rem)] shrink-0 truncate text-sm font-medium"
      >
        {command.label}
      </span>
      {#if command.hint !== undefined}
        <span
          class="text-text-tertiary ms-auto min-w-0 truncate ps-2 pe-1 text-xs"
        >
          {command.hint}
        </span>
      {/if}
    </button>
  {:else if entry.kind === "chip"}
    <button
      {...common}
      type="button"
      data-shape="pill"
      class="text-text-secondary relative h-8 shrink-0 rounded-full bg-(--ii-hover) px-3.5 text-sm font-medium whitespace-nowrap"
    >
      {entry.shelf.title}
    </button>
  {:else if entry.kind === "tile"}
    <button
      {...common}
      type="button"
      aria-label={`Open ${entry.app.name}`}
      class="relative flex w-[5.5rem] shrink-0 flex-col items-center gap-1.5 rounded-2xl px-1 pt-2.5 pb-2"
    >
      <AppLogo logo={entry.app.logo} size="tile" />
      <span class="text-text-primary w-full truncate text-center text-xs">
        {entry.app.name}
      </span>
    </button>
  {:else}
    {@const app = entry.app}
    <button
      {...common}
      type="button"
      aria-label={`${app.name}. ${verbOf(entry)}`}
      class="relative flex w-full items-center gap-3 rounded-2xl p-2 text-start"
    >
      {#if entry.kind === "link"}
        <span
          class="text-fg-tertiary grid size-12 shrink-0 place-items-center rounded-xl bg-(--ii-hover)"
        >
          <LinkIcon class="size-5" aria-hidden="true" />
        </span>
      {:else}
        <AppLogo logo={app.logo} size="lg" />
      {/if}
      <span class="flex min-w-0 flex-1 flex-col gap-0.5">
        <span class="text-text-primary truncate text-sm font-semibold">
          {app.name}
        </span>
        <span class="text-text-tertiary truncate text-xs">
          {entry.kind === "link"
            ? added(entry)
              ? "Added as a link"
              : "Add as a link"
            : app.oneLiner}
        </span>
      </span>
      {@render verbPill(entry)}
    </button>
  {/if}
{/snippet}

<div class="flex min-h-0 flex-1 flex-col">
  <div
    id={listId}
    role="listbox"
    aria-label={mode === "all" ? "Commands and apps" : "Apps"}
    class="min-h-0 flex-1 [scrollbar-width:none] overflow-y-auto overscroll-contain p-2 [&::-webkit-scrollbar]:hidden"
  >
    <div bind:this={content} class="relative">
      <!-- The highlight: one surface under the entries, gliding to the one
           the keys or the pointer are on, and changing its shape on the way
           (a pill for a chip). -->
      {#if mark !== undefined}
        <span
          aria-hidden="true"
          class={[
            "pointer-events-none absolute top-0 left-0 bg-(--ii-selected)",
            mark.glide &&
              !reduced &&
              "transition-[translate,width,height,border-radius] duration-220 ease-(--ii-ease-out)",
          ]}
          style:translate={`${mark.x}px ${mark.y}px`}
          style:width={`${mark.w}px`}
          style:height={`${mark.h}px`}
          style:border-radius={mark.radius}
        ></span>
      {/if}

      {#each groups as group (group.id)}
        <div
          role="group"
          aria-labelledby={group.title === undefined
            ? undefined
            : `${uid}-${group.id}`}
          aria-label={group.title === undefined ? "Browse" : undefined}
          class="flex flex-col"
        >
          {#if group.title !== undefined}
            <h3
              id={`${uid}-${group.id}`}
              class="text-text-tertiary px-2 pt-3 pb-1.5 text-xs font-medium"
            >
              {group.title}
            </h3>
          {/if}
          {#if group.layout === "chips" || group.layout === "tiles"}
            <!-- A row that scrolls sideways when it's wider than the panel
                 (a phone), its ends kept clear for the highlight. -->
            <div
              class={[
                "-mx-2 flex [scrollbar-width:none] overflow-x-auto px-2 [&::-webkit-scrollbar]:hidden",
                group.layout === "chips" ? "gap-1.5 py-1" : "gap-0.5",
              ]}
            >
              {#each group.entries as entry (entry.id)}
                {@render entryView(entry)}
              {/each}
            </div>
          {:else}
            {#each group.entries as entry (entry.id)}
              {@render entryView(entry)}
            {/each}
          {/if}
        </div>
      {:else}
        <div class="flex flex-col items-center gap-1 px-4 py-10 text-center">
          <p
            class="text-text-primary text-sm font-medium [overflow-wrap:anywhere]"
          >
            {mode === "all" ? "Nothing" : "No app"} called “{typed}”
          </p>
          <p class="text-text-tertiary text-sm text-pretty">
            {mode === "all"
              ? "Paste an app's link to add it."
              : "Paste its link to add it anyway."}
          </p>
        </div>
      {/each}
    </div>
  </div>

  {#if footer}
    <!-- The keys, and what Enter does on the row the highlight is on. -->
    <div
      aria-hidden="true"
      class="border-border-tertiary text-text-tertiary flex h-10 shrink-0 items-center gap-4 border-t px-4 text-xs"
    >
      <span class="flex items-center gap-1.5">
        <kbd class="kbd">↑</kbd><kbd class="kbd">↓</kbd> Move
      </span>
      {#if active !== undefined || (typed !== "" && flat.length > 0)}
        <span class="flex items-center gap-1.5">
          <kbd class="kbd">↵</kbd>
          {verbOf(active ?? flat[0])}
        </span>
      {/if}
      <span class="ms-auto flex items-center gap-1.5">
        <kbd class="kbd">esc</kbd>
        {shelf !== undefined || typed !== ""
          ? "Clear"
          : stepped
            ? "Back"
            : "Close"}
      </span>
    </div>
  {/if}
</div>

<style>
  .kbd {
    display: inline-grid;
    place-items: center;
    min-width: 1.25rem;
    height: 1.25rem;
    padding-inline: 0.3rem;
    border-radius: 0.375rem;
    border: 1px solid var(--color-border-tertiary);
    font-family: inherit;
    font-size: 0.6875rem;
    line-height: 1;
  }
</style>
