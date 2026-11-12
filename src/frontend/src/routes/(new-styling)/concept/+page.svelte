<script lang="ts">
  import "./surfaces.css";
  import {
    CheckIcon,
    ChevronsUpDownIcon,
    FlaskConicalIcon,
    LayoutGridIcon,
    PanelBottomIcon,
    PanelRightIcon,
    PanelTopIcon,
    XIcon,
  } from "@lucide/svelte";
  import type { Component } from "svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { MediaQuery } from "svelte/reactivity";
  import { slide } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import ActionDock, { type DockItem } from "./components/ActionDock.svelte";
  import AppTile from "./components/AppTile.svelte";
  import AppView from "./components/AppView.svelte";
  import BulkView from "./components/BulkView.svelte";
  import Elevated from "./components/Elevated.svelte";
  import Island, { type IslandView } from "./components/Island.svelte";
  import PanelHeader from "./components/PanelHeader.svelte";
  import ProgressiveBlur from "./components/ProgressiveBlur.svelte";
  import ReorderGrid from "./components/ReorderGrid.svelte";
  import SectionView, { type Section } from "./components/SectionView.svelte";
  import StatusRow from "./components/StatusRow.svelte";
  import CornerAccount from "./components/CornerAccount.svelte";
  import {
    browsersFor,
    featuredApps,
    identities,
    scenarioPresets,
    type ConceptApp,
  } from "./mockData";
  import { viewIn, viewOut } from "./motion";
  import {
    deriveStatus,
    isLoud,
    leadActionOf,
    leadOf,
    needsAttention,
    type CardKind,
    type CardStatus,
    type Scenario,
    type Tone,
  } from "./status";

  // --- Mock state, one per identity ---

  const states = $state(
    identities.map(({ apps, scenario }) => ({
      apps: apps.map((app) => ({ ...app, unread: [...app.unread] })),
      scenario: { ...scenario },
      signedOut: [] as string[],
    })),
  );
  let identityIndex = $state(0);

  const identity = $derived(identities[identityIndex].identity);
  const identityName = $derived(identity.name ?? `${identity.identityNumber}`);
  const signedInWith = $derived(identities[identityIndex].signedInWith);
  const current = $derived(states[identityIndex]);

  const browsers = $derived(
    browsersFor(current.scenario.devices).filter(
      ({ id }) => !current.signedOut.includes(id),
    ),
  );
  const cards = $derived(
    deriveStatus({
      scenario: current.scenario,
      signedOut: new Set(current.signedOut),
      browserCount: browsers.length,
      apps: {
        total: current.apps.length,
        notifying: current.apps.filter((app) => app.notificationsAllowed)
          .length,
      },
      signedInWith: signedInWith.title,
    }),
  );
  const lead = $derived(leadOf(cards));
  /** What the calm island mentions: something new or waiting, never a tip. */
  const note = $derived(
    cards.find(({ tone }) =>
      (["queued", "pending", "now", "recent"] as const).some((t) => t === tone),
    ),
  );

  // --- Apps ---

  // The grid keeps the order apps were added in, so a launch never moves a
  // tile. What was used last gets its own short row above it.
  const recentApps = $derived(
    [...current.apps]
      .sort((a, b) => b.lastUsedMillis - a.lastUsedMillis)
      .slice(0, 4),
  );
  const showRecent = $derived(current.apps.length > 6);
  const featured = $derived(
    featuredApps.filter(
      ({ origin }) => !current.apps.some((app) => app.origin === origin),
    ),
  );

  // Columns follow the canvas the way Apple's home screens do: four on a
  // phone, five on a tablet, seven on a desktop (the Mac's Applications
  // grid). Recent and All apps share them, so they line up tile for tile.
  const GRID =
    "grid grid-cols-[repeat(4,5.25rem)] gap-x-1 gap-y-1 @lg:grid-cols-[repeat(4,7rem)] @lg:gap-x-2 @2xl:grid-cols-[repeat(5,7rem)] @4xl:grid-cols-[repeat(7,7rem)]";
  // A heading starts at the edge of the first logo, like a shelf title on
  // iOS. The 4rem logo sits centered in its column, so the inset is half of
  // what the column leaves: (5.25rem − 4rem) / 2, then (7rem − 4rem) / 2.
  const INSET = "ps-2.5 @lg:ps-6";

  // --- Layout: the concept compares three ---

  type Layout = "panel" | "island" | "top";
  // The account on top is the concept's main layout; the other two stay in
  // the concept menu to compare against.
  const LAYOUTS: { value: Layout; label: string; icon: Component }[] = [
    {
      value: "top",
      label: "Account on top, widgets below",
      icon: PanelTopIcon,
    },
    { value: "island", label: "Island at the bottom", icon: PanelBottomIcon },
    { value: "panel", label: "Panel on the side", icon: PanelRightIcon },
  ];
  // A new key, so a layout picked before the account moved up doesn't stand
  // in for the main one.
  const LAYOUT_KEY = "ii-concept-layout-v2";
  // `?layout=island` in the address picks a layout too, so a link can show one.
  const readLayout = (): Layout => {
    const asked =
      new URLSearchParams(location.search).get("layout") ??
      localStorage.getItem(LAYOUT_KEY);
    return LAYOUTS.find(({ value }) => value === asked)?.value ?? "top";
  };
  let layout = $state<Layout>(readLayout());
  const isDesktop = new MediaQuery("min-width: 768px");
  /** The panel the island or the account opens on desktop, and the sheet on
   *  a phone. */
  let panelOpen = $state(false);
  const panelVisible = $derived(
    isDesktop.current ? layout === "panel" || panelOpen : panelOpen,
  );
  /** The account's pill in the top layout, for the room the header keeps. */
  let accountWidth = $state(0);
  let accountHeight = $state(0);

  const setLayout = (next: Layout) => {
    layout = next;
    localStorage.setItem(LAYOUT_KEY, layout);
    panelOpen = false;
  };

  /** The concept's one menu (layout and scenario), and where it was opened:
   *  the corner on desktop opens it upwards, the header downwards. */
  let conceptMenu = $state<{ anchor: HTMLElement; compact: boolean }>();

  // --- Panel ---

  const TITLES: Record<CardKind, string> = {
    access: "Access methods",
    devices: "Devices",
    recovery: "Recovery",
    apps: "Manage apps",
    integrations: "Integrations",
  };

  let section = $state<Section>();
  let managing = $state(false);
  let selected = $state<string[]>([]);
  let isSwitcherOpen = $state(false);
  let switcherAnchor = $state<HTMLElement>();

  const selectedApps = $derived(
    selected
      .map((origin) => current.apps.find((app) => app.origin === origin))
      .filter((app) => app !== undefined),
  );

  // Which view the panel shows, and which way it moved to get there.
  const viewKey = $derived(
    managing
      ? selectedApps.length === 1
        ? "app"
        : selectedApps.length > 1
          ? "bulk"
          : "manage"
      : (section ?? "overview"),
  );
  let navDirection = $state(1);
  const readingDirection = () =>
    document.documentElement.dir === "rtl" ? -1 : 1;
  const reduced = $derived(prefersReducedMotion.current);

  const openCard = (kind: CardKind) => {
    navDirection = 1;
    if (kind === "apps") {
      startManaging();
    } else {
      section = kind;
    }
  };

  const closeSection = () => {
    navDirection = -1;
    section = undefined;
  };

  const startManaging = () => {
    navDirection = 1;
    section = undefined;
    selected = [];
    managing = true;
    // On a phone the sheet steps aside, so the grid is there to pick from.
    if (!isDesktop.current) {
      panelOpen = false;
    }
  };

  /** Managing from a tile's menu, like a home screen's edit mode: the grid
   *  starts to rock with that app already picked. */
  const manageFrom = (app: ConceptApp) => {
    startManaging();
    selected = [app.origin];
  };

  const stopManaging = () => {
    navDirection = -1;
    managing = false;
    selected = [];
    if (!isDesktop.current) {
      panelOpen = false;
    }
  };

  const toggleSelected = (origin: string) => {
    const before = selected.length;
    selected = selected.includes(origin)
      ? selected.filter((one) => one !== origin)
      : [...selected, origin];
    navDirection = selected.length > before ? 1 : -1;
  };

  const islandView = $derived<IslandView | undefined>(
    managing
      ? panelVisible
        ? undefined
        : "select"
      : layout === "island" && !panelVisible
        ? lead !== undefined
          ? "lead"
          : "calm"
        : undefined,
  );

  // --- Top layout: the account in the header, what needs you below ---

  /** What earns a widget, most urgent first: a risk, the step to take, steps
   *  waiting behind it, something on its way, and anything new. */
  const URGENCY: Tone[] = ["risk", "action", "queued", "pending", "recent"];
  const attention = $derived(
    cards
      .filter(({ tone }) => URGENCY.includes(tone))
      .sort((a, b) => URGENCY.indexOf(a.tone) - URGENCY.indexOf(b.tone)),
  );
  const dockItems = $derived<DockItem[]>(
    attention.slice(0, 3).map((card) => ({
      kind: card.kind,
      title: TITLES[card.kind],
      sentence: card.sentence,
      tone: card.tone,
      action: leadActionOf(card, current.scenario),
      primary: isLoud(card),
    })),
  );
  const accountStatus = $derived(
    attention.length === 0
      ? undefined
      : {
          sentence:
            attention.length === 1
              ? "1 thing to review"
              : `${attention.length} things to review`,
          tone: attention[0].tone,
        },
  );

  /** The account opens on its overview, unless apps are being managed. */
  const openAccount = () => {
    if (!managing) {
      section = undefined;
    }
    panelOpen = true;
  };

  /** A widget opens the account right at its section. */
  const openFromDock = (kind: CardKind) => {
    openCard(kind);
    panelOpen = true;
  };

  const actFromDock = (kind: CardKind) => {
    const card = cards.find((one) => one.kind === kind);
    if (card !== undefined) {
      runLead(card);
    }
  };

  // --- Apps grid pages ---

  const PAGES = [
    { id: "mine", label: "Your apps" },
    { id: "explore", label: "Explore" },
    { id: "create", label: "Create" },
  ] as const;
  let activePage = $state(0);
  let pager = $state<HTMLElement>();
  const pageRefs = $state<HTMLElement[]>([]);
  const tabRefs = $state<HTMLElement[]>([]);

  const goToPage = (index: number) => {
    activePage = index;
    pageRefs[index]?.scrollIntoView({
      behavior: reduced ? "instant" : "smooth",
      block: "nearest",
      inline: "start",
    });
  };

  const onTabKeydown = (event: KeyboardEvent) => {
    const step = readingDirection();
    const next =
      event.key === "ArrowRight"
        ? activePage + step
        : event.key === "ArrowLeft"
          ? activePage - step
          : event.key === "Home"
            ? 0
            : event.key === "End"
              ? PAGES.length - 1
              : undefined;
    if (next === undefined || next < 0 || next >= PAGES.length) return;
    event.preventDefault();
    goToPage(next);
    tabRefs[next]?.focus();
  };

  // A swipe or a trackpad scroll lands on a page; the tabs follow it.
  $effect(() => {
    if (pager === undefined) return;
    const observer = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          if (entry.isIntersecting) {
            activePage = pageRefs.indexOf(entry.target as HTMLElement);
          }
        }
      },
      { root: pager, threshold: 0.6 },
    );
    for (const page of pageRefs) {
      observer.observe(page);
    }
    return () => observer.disconnect();
  });

  // --- Actions on the mock ---

  const unavailable = (step: string) =>
    toaster.create({
      title: `${step} isn't part of this concept yet`,
      duration: 3000,
    });

  /** Kinds that were just resolved, so their row shows a check for a moment. */
  let resolvedKinds = $state<CardKind[]>([]);

  const resolve = (patch: Partial<Scenario>, message: string) => {
    Object.assign(current.scenario, patch);
    const kinds = Object.keys(patch) as CardKind[];
    resolvedKinds = [...resolvedKinds, ...kinds];
    setTimeout(() => {
      resolvedKinds = resolvedKinds.filter((kind) => !kinds.includes(kind));
    }, 1800);
    toaster.success({ title: message, duration: 2500 });
  };

  /** The loud card's button: finishes the step where the concept can, otherwise
   *  opens the card's section. */
  const runLead = (card: CardStatus) => {
    const action = leadActionOf(card, current.scenario);
    if (action.resolve !== undefined) {
      resolve(action.resolve.patch, action.resolve.message);
      return;
    }
    openCard(card.kind);
    panelOpen = true;
  };

  const applyPreset = (index: number) => {
    current.scenario = { ...scenarioPresets[index].scenario };
    current.signedOut = [];
    navDirection = -1;
    section = undefined;
  };

  const signOut = (id: string) => {
    current.signedOut = [...current.signedOut, id];
    toaster.success({ title: "Signed out", duration: 2500 });
  };

  const readAll = (app: ConceptApp) => {
    app.unread = [];
  };

  const read = (app: ConceptApp, id: string) => {
    app.unread = app.unread.filter((notification) => notification.id !== id);
  };

  const setNotifications = (apps: ConceptApp[], allowed: boolean) => {
    for (const app of apps) {
      app.notificationsAllowed = allowed;
    }
  };

  /** The tile's menu changes a setting the grid doesn't show, so it says so. */
  const setNotificationsFromMenu = (app: ConceptApp, allowed: boolean) => {
    setNotifications([app], allowed);
    toaster.success({
      title: `Notifications ${allowed ? "on" : "off"} for ${app.name}`,
      duration: 2500,
    });
  };

  /** Dragging a tile while managing rearranges the grid. */
  const moveApp = (from: number, to: number) => {
    const apps = [...current.apps];
    const [moved] = apps.splice(from, 1);
    apps.splice(to, 0, moved);
    current.apps = apps;
  };

  const remove = (apps: ConceptApp[]) => {
    const state = current;
    const before = [...state.apps];
    const origins = apps.map(({ origin }) => origin);
    state.apps = state.apps.filter(({ origin }) => !origins.includes(origin));
    navDirection = -1;
    selected = [];
    toaster.create({
      title:
        apps.length === 1
          ? `${apps[0].name} has been removed.`
          : `${apps.length} apps have been removed.`,
      description: "You've been signed out and they can no longer notify you.",
      closable: true,
      duration: 5000,
      action: { label: "Undo", onClick: () => (state.apps = before) },
    });
  };

  const switchIdentity = (identityNumber: bigint): Promise<void> => {
    identityIndex = identities.findIndex(
      (entry) => entry.identity.identityNumber === identityNumber,
    );
    isSwitcherOpen = false;
    section = undefined;
    stopManaging();
    return Promise.resolve();
  };

  const onWindowKeydown = (event: KeyboardEvent) => {
    if (
      event.key === "Escape" &&
      !event.defaultPrevented &&
      (layout === "island" || layout === "top") &&
      isDesktop.current &&
      panelOpen &&
      !isSwitcherOpen &&
      // An open popover takes Escape first.
      document.querySelector(".popover:popover-open") === null
    ) {
      panelOpen = false;
    }
  };
</script>

<svelte:window onkeydown={onWindowKeydown} />

{#snippet appTile(app: ConceptApp, index: number)}
  <AppTile
    name={app.name}
    logo={app.logo}
    url={app.url}
    unread={app.unread}
    mode={managing ? "select" : "open"}
    selected={selected.includes(app.origin)}
    {index}
    onToggle={() => toggleSelected(app.origin)}
    onOpen={() => readAll(app)}
    onRead={(id) => read(app, id)}
    onClearAll={() => readAll(app)}
    notificationsAllowed={app.notificationsAllowed}
    onSetNotifications={(allowed) => setNotificationsFromMenu(app, allowed)}
    onManage={() => manageFrom(app)}
    onRemove={() => remove([app])}
  />
{/snippet}

{#snippet conceptControls(compact: boolean)}
  <!-- Concept only, so it stays out of the way: a bare grey glyph that opens
       one menu with the layout and the scenario. -->
  <button
    type="button"
    title="Concept settings"
    aria-label="Concept settings"
    aria-haspopup="menu"
    aria-expanded={conceptMenu !== undefined}
    onclick={(event) =>
      (conceptMenu = { anchor: event.currentTarget, compact })}
    class="text-fg-quaternary hover:text-fg-tertiary focus-visible:outline-focus-ring grid size-8 shrink-0 place-items-center rounded-full transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2 active:bg-(--ii-selected)"
  >
    <FlaskConicalIcon class="size-4" aria-hidden="true" />
  </button>
{/snippet}

{#snippet overview(closable: boolean, inSheet: boolean, cornered: boolean)}
  <div class="flex flex-col gap-3">
    <!-- With the account in the corner, the account itself is this header. -->
    {#if !cornered}
      <!-- The header stands on the panel's edge, not on the icons inside the
         rows: the avatar lines up with the list below. With no inset there is
         no room for a hover fill, so the switch control lights up instead. -->
      <div class={["flex items-center gap-1", inSheet && "pe-10"]}>
        <button
          bind:this={switcherAnchor}
          type="button"
          onclick={() => (isSwitcherOpen = true)}
          aria-label={`${identityName}, identity ${identity.identityNumber}. Switch identity`}
          class="group/identity focus-visible:outline-focus-ring flex min-w-0 flex-1 items-center gap-3 rounded-2xl py-2 text-start focus-visible:outline-2 focus-visible:outline-offset-4"
        >
          <IdentityAvatar {identity} size="sm" />
          <!-- The panel speaks in two sizes and two weights: a 14px medium
             label over a 13px regular line, the same as every row below. -->
          <span class="flex min-w-0 flex-1 flex-col">
            <span class="text-text-primary truncate text-sm font-medium">
              {identityName}
            </span>
            <span
              class="text-text-tertiary text-[13px] leading-[18px] tabular-nums"
            >
              Identity {identity.identityNumber}
            </span>
          </span>
          <span
            aria-hidden="true"
            class="text-fg-tertiary grid size-8 shrink-0 place-items-center rounded-full transition-colors duration-150 group-hover/identity:bg-(--ii-hover) group-active/identity:bg-(--ii-selected)"
          >
            <ChevronsUpDownIcon class="size-4" />
          </span>
        </button>
        {#if closable}
          <!-- Drawn like the switch control beside it: same circle, same 16px
             glyph, same muted tone, so neither outweighs the other. -->
          <button
            type="button"
            onclick={() => (panelOpen = false)}
            class="text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring grid size-8 shrink-0 place-items-center rounded-full transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2 active:bg-(--ii-selected)"
          >
            <XIcon class="size-4" aria-hidden="true" />
            <span class="sr-only">Close</span>
          </button>
        {/if}
      </div>
    {/if}
    <Elevated
      offset={1}
      as="ul"
      class="divide-border-tertiary flex flex-col divide-y overflow-hidden rounded-2xl"
    >
      {#each cards as card (card.kind)}
        <StatusRow
          kind={card.kind}
          title={TITLES[card.kind]}
          sentence={card.sentence}
          tone={card.tone}
          action={isLoud(card)
            ? leadActionOf(card, current.scenario)
            : undefined}
          resolved={resolvedKinds.includes(card.kind)}
          onOpen={() => openCard(card.kind)}
          onAction={() => runLead(card)}
        />
      {/each}
    </Elevated>
  </div>
{/snippet}

{#snippet panelBody(closable: boolean, inSheet: boolean, cornered: boolean)}
  {#if managing}
    {#if selectedApps.length === 1}
      {@const app = selectedApps[0]}
      <AppView
        {app}
        onExit={stopManaging}
        onToggleNotifications={(allowed) => setNotifications([app], allowed)}
        onRemove={() => remove([app])}
      />
    {:else if selectedApps.length > 1}
      <BulkView
        apps={selectedApps}
        onExit={stopManaging}
        onSetNotifications={(allowed) =>
          setNotifications(selectedApps, allowed)}
        onRemove={() => remove(selectedApps)}
      />
    {:else}
      <PanelHeader
        title="Manage apps"
        lead="Select apps to change their permissions or remove them."
        backLabel="Done"
        onBack={stopManaging}
      />
    {/if}
  {:else if section !== undefined}
    <SectionView
      {section}
      scenario={current.scenario}
      {signedInWith}
      {browsers}
      onBack={closeSection}
      onSignOut={signOut}
      onResolve={resolve}
      onUnavailable={unavailable}
    />
  {:else}
    {@render overview(closable, inSheet, cornered)}
  {/if}
{/snippet}

{#snippet panel(closable: boolean, inSheet: boolean, cornered: boolean)}
  {#if cornered}
    <!-- The account's row, kept free for it as it arrives from the canvas,
         and kept clear when the panel scrolls under it. -->
    <div
      aria-hidden="true"
      class="ii-surface-1 sticky top-0 z-10 -mt-1 mb-1 h-15 shrink-0"
    ></div>
  {/if}
  <!-- Views stack in one cell, so the old one fades while the new one arrives. -->
  <div class="grid">
    {#key viewKey}
      <div
        class="col-start-1 row-start-1 min-w-0"
        in:viewIn={{ direction: navDirection * readingDirection(), reduced }}
        out:viewOut={{ reduced }}
      >
        {@render panelBody(closable, inSheet, cornered)}
      </div>
    {/key}
  </div>
{/snippet}

<div
  class="concept ii-surface-1 text-text-primary min-h-dvh md:flex md:h-dvh md:p-2"
>
  <!-- The canvas: the apps, in a frame on the page. -->
  <Elevated
    offset={2}
    as="main"
    class="@container flex min-h-dvh min-w-0 flex-1 flex-col max-md:rounded-none max-md:border-0 max-md:shadow-none md:min-h-0 md:overflow-y-auto md:rounded-[1.25rem]"
  >
    <div class="h-[env(safe-area-inset-top)] shrink-0"></div>
    <h1 class="sr-only">Apps</h1>

    <!-- On desktop one row: the logo, the pages centered on the canvas, and an
         empty column that keeps them centered. A phone has no room for the
         pages beside its controls, so they drop to a row of their own. -->
    <header
      class="grid shrink-0 grid-cols-[1fr_auto] items-center gap-x-1 px-4 sm:px-6 md:h-16 md:grid-cols-[1fr_auto_1fr]"
    >
      <div dir="ltr" class="flex h-16 items-center gap-3 justify-self-start">
        <Logo class="text-fg-primary h-4" />
        <span class="text-text-primary text-base font-semibold"> id.ai </span>
      </div>
      <div
        role="tablist"
        aria-label="Apps"
        tabindex="-1"
        onkeydown={onTabKeydown}
        class="col-span-2 row-start-2 mt-2 flex gap-6 justify-self-center md:col-span-1 md:col-start-2 md:row-start-1 md:mt-0"
      >
        <!-- No underline: the selected tab reads by its darker text alone. -->
        {#each PAGES as page, index (page.id)}
          <button
            bind:this={tabRefs[index]}
            type="button"
            role="tab"
            id={`tab-${page.id}`}
            aria-selected={activePage === index}
            aria-controls={`page-${page.id}`}
            tabindex={activePage === index ? 0 : -1}
            onclick={() => goToPage(index)}
            class={[
              "focus-visible:outline-focus-ring rounded-sm py-1 text-sm font-semibold transition-colors duration-150 ease-out focus-visible:outline-2 focus-visible:outline-offset-2 motion-reduce:transition-none",
              activePage === index
                ? "text-text-primary"
                : "text-text-tertiary hover:text-text-secondary",
            ]}
          >
            {page.label}
          </button>
        {/each}
      </div>
      <div
        class={[
          "col-start-2 row-start-1 flex items-center gap-1 justify-self-end md:col-start-3",
          layout !== "top" && "md:hidden",
        ]}
      >
        <!-- Phone only: on desktop it keeps to the bottom-left corner. -->
        <div class="flex md:hidden">
          {@render conceptControls(true)}
        </div>
        {#if layout === "top"}
          <!-- Desktop: room for the account, which floats over this corner
               (CornerAccount). It folds away while the account sits on the
               panel, and opens again for it to come back. -->
          <div
            aria-hidden="true"
            class="shrink-0 transition-[width,margin] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none max-md:hidden"
            style:width={`${panelVisible ? 0 : accountWidth}px`}
            style:height={`${accountHeight}px`}
            style:margin-inline-start={panelVisible ? "0px" : "0.5rem"}
          ></div>
        {/if}
        {#if layout === "panel" || layout === "top"}
          <!-- Phone: the panel opens from here; the dot says it has something for you. -->
          <button
            type="button"
            onclick={() => (panelOpen = true)}
            aria-label={`Open ${identityName}${needsAttention(cards) ? ", needs your attention" : ""}`}
            class="relative -me-2 flex size-11 items-center justify-center rounded-full md:hidden"
          >
            <IdentityAvatar {identity} size="sm" />
            {#if needsAttention(cards)}
              <span
                aria-hidden="true"
                class="bg-bg-warning-solid absolute end-1 top-1 size-3 rounded-full ring-2 ring-(color:--ii-surface-3)"
              ></span>
            {/if}
          </button>
        {/if}
      </div>
    </header>

    <!-- On a phone the grid starts at the top like a home screen; on desktop it
         sits in the middle of the free space, and the room kept for the island
         below lifts it a little above center. -->
    <div class="relative flex flex-1 flex-col md:my-auto md:flex-none">
      <!-- The pages side by side: the tabs move between them, and on touch or a
         trackpad a swipe does too, landing on a page. -->
      <div
        bind:this={pager}
        class="flex flex-1 snap-x snap-mandatory [scrollbar-width:none] overflow-x-auto overscroll-x-contain md:flex-none [&::-webkit-scrollbar]:hidden"
      >
        <div
          bind:this={pageRefs[0]}
          id="page-mine"
          role="tabpanel"
          aria-labelledby="tab-mine"
          class="w-full shrink-0 snap-start snap-always px-2 pt-6 pb-32 sm:px-6"
        >
          {#if current.apps.length > 0}
            <div class="mx-auto flex w-fit flex-col">
              <!-- Recent is ordered by use, not by hand, so it steps aside while
                   apps are managed and All apps is the one grid to arrange. -->
              {#if showRecent && !managing}
                <section
                  transition:slide={{
                    duration: reduced ? 0 : 260,
                    easing: quintOut,
                  }}
                  class="flex flex-col gap-2 pb-8"
                >
                  <h2 class={["text-text-tertiary text-sm font-medium", INSET]}>
                    Recent
                  </h2>
                  <ul class={GRID}>
                    {#each recentApps as app, index (app.origin)}
                      <li>{@render appTile(app, index)}</li>
                    {/each}
                  </ul>
                </section>
              {/if}
              <section class="flex flex-col gap-2">
                <h2 class={["text-text-tertiary text-sm font-medium", INSET]}>
                  All apps
                </h2>
                <ReorderGrid
                  items={current.apps}
                  key={(app) => app.origin}
                  editing={managing}
                  onMove={moveApp}
                  class={GRID}
                >
                  {#snippet tile(app, index)}
                    {@render appTile(
                      app,
                      index + (showRecent && !managing ? 4 : 0),
                    )}
                  {/snippet}
                </ReorderGrid>
              </section>
            </div>
          {:else}
            <div
              class="border-border-tertiary mx-auto flex max-w-md flex-col items-center justify-center gap-2 rounded-2xl border border-dashed px-6 py-10 text-center"
            >
              <LayoutGridIcon
                class="text-fg-quaternary size-7"
                aria-hidden="true"
              />
              <p class="text-text-tertiary text-sm text-pretty">
                Apps you sign in to with this identity show up here.
              </p>
              <button
                type="button"
                onclick={() => goToPage(1)}
                class="btn btn-secondary btn-sm mt-2 rounded-full"
              >
                Explore apps
              </button>
            </div>
          {/if}
        </div>

        <div
          bind:this={pageRefs[1]}
          id="page-explore"
          role="tabpanel"
          aria-labelledby="tab-explore"
          class="w-full shrink-0 snap-start snap-always px-2 pt-6 pb-32 sm:px-6"
        >
          <div class="mx-auto flex w-fit flex-col gap-2">
            <h2 class={["text-text-tertiary text-sm font-medium", INSET]}>
              Featured on Internet Identity
            </h2>
            {#if featured.length > 0}
              <ul class={GRID}>
                {#each featured as app (app.origin)}
                  <li>
                    <AppTile
                      name={app.name}
                      logo={app.logo}
                      url={app.url}
                      mode={managing ? "inert" : "open"}
                    />
                  </li>
                {/each}
              </ul>
            {:else}
              <p class={["text-text-tertiary text-sm", INSET]}>
                You already use every featured app.
              </p>
            {/if}
          </div>
        </div>

        <!-- Create: a page of its own, empty for now. -->
        <div
          bind:this={pageRefs[2]}
          id="page-create"
          role="tabpanel"
          aria-labelledby="tab-create"
          class="w-full shrink-0 snap-start snap-always px-2 pt-6 pb-32 sm:px-6"
        ></div>
      </div>
      <!-- Tiles blur deeper the nearer they get to either edge while a page
           slides. Each side takes all the room up to the first logo, so tiles
           at rest stay sharp: half of what GRID leaves (21.75rem wide, 29.5rem
           from @lg, 37rem from @2xl, 52rem from @4xl) plus the logo's inset
           in its column, less a hair. -->
      <ProgressiveBlur
        side="left"
        class="w-[calc((100%_-_21.75rem)/2_+_0.5rem)] @lg:w-[calc((100%_-_29.5rem)/2_+_1.25rem)] @2xl:w-[calc((100%_-_37rem)/2_+_1.25rem)] @4xl:w-[calc((100%_-_52rem)/2_+_1.25rem)]"
      />
      <ProgressiveBlur
        side="right"
        class="w-[calc((100%_-_21.75rem)/2_+_0.5rem)] @lg:w-[calc((100%_-_29.5rem)/2_+_1.25rem)] @2xl:w-[calc((100%_-_37rem)/2_+_1.25rem)] @4xl:w-[calc((100%_-_52rem)/2_+_1.25rem)]"
      />
    </div>
  </Elevated>

  <!-- Desktop: the panel lies on the page beside the canvas. With the island it
       opens and closes; the canvas gives it the room. -->
  <div
    class="h-full shrink-0 overflow-hidden transition-[width] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none max-md:hidden"
    style:width={panelVisible ? "25rem" : "0rem"}
  >
    <aside
      aria-label="Identity"
      inert={!panelVisible}
      class={[
        // 24px of air on both sides: from the canvas frame (8px gap + 16px)
        // and from the window edge (8px page padding + 16px).
        "h-full w-[25rem] overflow-y-auto overscroll-contain ps-6 pe-4 pt-1 pb-4",
        "transition-[opacity,translate] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
        !panelVisible && "translate-x-4 opacity-0 rtl:-translate-x-4",
      ]}
    >
      {#if isDesktop.current}
        {@render panel(layout === "island", false, layout === "top")}
      {/if}
    </aside>
  </div>

  {#if layout === "top" && isDesktop.current}
    <!-- The account in the top-right corner, over the canvas or on the panel. -->
    <CornerAccount
      open={panelOpen}
      {identity}
      status={accountStatus}
      bind:width={accountWidth}
      bind:height={accountHeight}
      onOpen={openAccount}
      onClose={() => (panelOpen = false)}
      onSwitch={(anchor) => {
        switcherAnchor = anchor;
        isSwitcherOpen = true;
      }}
    />
  {/if}

  <!-- Phone: the panel is a sheet from the bottom, as dialogs are elsewhere. -->
  {#if panelOpen && !isDesktop.current}
    <Dialog onClose={() => (panelOpen = false)}>
      {@render panel(false, true, false)}
    </Dialog>
  {/if}

  {#if islandView !== undefined}
    <Island
      view={islandView}
      {identity}
      note={note !== undefined
        ? { sentence: note.sentence, tone: note.tone }
        : undefined}
      lead={lead !== undefined
        ? {
            title: TITLES[lead.kind],
            sentence: lead.sentence,
            tone: lead.tone,
            action: leadActionOf(lead, current.scenario),
          }
        : undefined}
      selectedCount={selected.length}
      onOpen={() => (panelOpen = true)}
      onAction={() => lead !== undefined && runLead(lead)}
      onDetails={() => (panelOpen = true)}
      onDone={stopManaging}
    />
  {/if}

  {#if layout === "top" && !panelVisible && !managing && dockItems.length > 0}
    <ActionDock
      items={dockItems}
      onOpen={openFromDock}
      onAction={actFromDock}
    />
  {/if}

  <!-- Concept only: walk the panel through its states, and switch layouts.
       On desktop it stays in the bottom-left corner in every layout; on a
       phone it sits in the header, leaving the bottom to the island. -->
  <div class="fixed start-4 bottom-4 z-10 max-md:hidden">
    {@render conceptControls(false)}
  </div>

  {#if conceptMenu !== undefined}
    {@const close = () => (conceptMenu = undefined)}
    <Popover
      anchor={conceptMenu.anchor}
      onClose={close}
      direction={conceptMenu.compact ? "down" : "up"}
      align={conceptMenu.compact ? "end" : "start"}
      distance="0.5rem"
      responsive={false}
      class="!w-max p-1.5 !shadow-lg"
    >
      <div class="flex flex-col" role="menu" aria-label="Concept settings">
        <div
          aria-hidden="true"
          class="text-text-tertiary px-3 pt-1.5 pb-1 text-xs"
        >
          Layout
        </div>
        {#each LAYOUTS as option (option.value)}
          {@const Icon = option.icon}
          <button
            type="button"
            role="menuitemradio"
            aria-checked={option.value === layout}
            onclick={() => {
              close();
              setLayout(option.value);
            }}
            class="btn btn-tertiary justify-start gap-2.5 !px-3 text-start"
          >
            <Icon
              class="text-fg-quaternary dark:text-fg-tertiary size-4"
              aria-hidden="true"
            />
            <span class="flex-1">{option.label}</span>
            <CheckIcon
              class={`text-fg-tertiary size-4 ${option.value === layout ? "" : "invisible"}`}
              aria-hidden="true"
            />
          </button>
        {/each}
        <hr class="border-border-secondary -mx-1.5 my-1.5" />
        <div
          aria-hidden="true"
          class="text-text-tertiary px-3 pt-1.5 pb-1 text-xs"
        >
          Scenario
        </div>
        {#each scenarioPresets as preset, index (index)}
          <button
            type="button"
            role="menuitem"
            onclick={() => {
              close();
              applyPreset(index);
            }}
            class="btn btn-tertiary justify-start !px-3 text-start"
          >
            {preset.label}
          </button>
        {/each}
      </div>
    </Popover>
  {/if}

  {#if isSwitcherOpen}
    <Popover
      anchor={switcherAnchor}
      onClose={() => (isSwitcherOpen = false)}
      direction="down"
      align="start"
      distance="0.5rem"
      class="!bg-bg-primary"
    >
      <IdentitySwitcher
        selected={identity.identityNumber}
        identities={identities.map((entry) => entry.identity)}
        onSwitchIdentity={switchIdentity}
        onUseAnotherIdentity={() => {
          isSwitcherOpen = false;
          unavailable("Adding an identity");
        }}
        onError={() => (isSwitcherOpen = false)}
        onClose={() => (isSwitcherOpen = false)}
        onSignOut={() => {
          isSwitcherOpen = false;
          unavailable("Signing out");
          return Promise.resolve();
        }}
      />
    </Popover>
  {/if}
</div>
