<script lang="ts">
  import "./foundation/surfaces/surfaces.css";
  import "./foundation/motion/loading.css";
  import {
    ArrowRightToLineIcon,
    BellOffIcon,
    BotIcon,
    BriefcaseMedicalIcon,
    CheckIcon,
    ChevronRightIcon,
    CompassIcon,
    FlaskConicalIcon,
    Grid2x2CheckIcon,
    KeyRoundIcon,
    LogOutIcon,
    PanelBottomIcon,
    PanelRightIcon,
    PanelTopIcon,
    MonitorSmartphoneIcon,
    MoonIcon,
    PlusIcon,
    SunIcon,
  } from "@lucide/svelte";
  import { tick, untrack, type Component } from "svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { MediaQuery } from "svelte/reactivity";
  import { fade, slide } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import {
    IDENTITY_PALETTES,
    identityInitial,
    identityPalette,
    identityPaletteIndex,
    type IdentityPalette,
  } from "$lib/utils/identityColors";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import ActionDock, {
    type DockItem,
  } from "./foundation/frame/ActionDock.svelte";
  import AppTile from "./features/apps/AppTile.svelte";
  import AttentionDot from "./foundation/frame/AttentionDot.svelte";
  import AppView from "./features/apps/AppView.svelte";
  import BulkView from "./features/apps/BulkView.svelte";
  import ConnectPage from "./features/connect/ConnectPage.svelte";
  import Elevated, {
    CANVAS,
    surfaceColor,
  } from "./foundation/surfaces/Elevated.svelte";
  import HeadCard from "./features/security/HeadCard.svelte";
  import HeaderSearch from "./features/search/HeaderSearch.svelte";
  import SurfaceLevel from "./foundation/surfaces/SurfaceLevel.svelte";
  import Popup from "./foundation/frame/Popup.svelte";
  import AppRow from "./features/apps/AppRow.svelte";
  import ExplorePage from "./features/explore/ExplorePage.svelte";
  import ColourStrip from "./features/identity/ColourStrip.svelte";
  import IdentityCard from "./features/identity/IdentityCard.svelte";
  import IdentityList from "./features/identity/IdentityList.svelte";
  import IdentityMenu from "./features/identity/IdentityMenu.svelte";
  import IncidentBanner from "./features/security/IncidentBanner.svelte";
  import IncidentCard from "./features/security/IncidentCard.svelte";
  import Island, { type IslandView } from "./foundation/frame/Island.svelte";
  import ManageHints from "./features/apps/ManageHints.svelte";
  import PanelCard from "./foundation/frame/PanelCard.svelte";
  import PanelHeader from "./foundation/frame/PanelHeader.svelte";
  import ProgressiveBlur from "./foundation/motion/ProgressiveBlur.svelte";
  import SearchOverlay from "./features/search/SearchOverlay.svelte";
  import Sheet from "./foundation/frame/Sheet.svelte";
  import RecoveryFlow, {
    type RecoveryMethod,
  } from "./features/security/RecoveryFlow.svelte";
  import ReorderGrid from "./features/apps/ReorderGrid.svelte";
  import SectionView, {
    type Section,
  } from "./features/security/SectionView.svelte";
  import StateBoard from "./mock/StateBoard.svelte";
  import StatusCard from "./features/security/StatusCard.svelte";
  import Swap from "./foundation/motion/Swap.svelte";
  import { glyphsOf } from "./features/security/glyphs";
  import {
    LOAD_SPEEDS,
    LOAD_SPEED_KEY,
    Load,
    loadSpeedOf,
    saveLoadSpeed,
    type LoadSpeed,
  } from "./foundation/motion/loading.svelte";
  import {
    browsersFor,
    featuredApps,
    identities,
    scenarioPresets,
    type ConceptApp,
    type FeaturedApp,
  } from "./mock/mockData";
  import {
    arrive as arriveIn,
    morph,
    pageArrive,
    pageLeave,
    shellIn,
    shellOut,
    spotsOf,
    viewIn,
    viewOut,
    type Spot,
  } from "./foundation/motion/motion";
  import { ConceptTheme } from "./mock/theme.svelte";
  import {
    deriveStatus,
    incidentOf,
    isLoud,
    leadActionOf,
    leadOf,
    needsAttention,
    type CardKind,
    type CardStatus,
    type IncidentAnswer,
    type Scenario,
    type Tone,
  } from "./features/security/status";

  // --- Mock state, one per identity ---

  const states = $state(
    identities.map(({ apps, recentOnly, scenario }) => ({
      apps: apps.map((app) => ({ ...app, unread: [...app.unread] })),
      recentOnly: [...recentOnly],
      scenario: { ...scenario },
      signedOut: [] as string[],
      /** Not in Internet Identity today: whether the identity keeps a record
       *  of the apps it signs in to. On, a signed-in app joins My apps and
       *  history is kept; off, there's no record of where it signs in, so My
       *  apps holds only what the user added (or the preset it started
       *  with). One per identity, as it would be saved with it. */
      keepHistory: true,
    })),
  );
  let identityIndex = $state(0);

  /** Colours picked for each identity, by their place in IDENTITY_PALETTES.
   *  Where a picked colour would be saved is still open. The name isn't
   *  picked here: passkeys have no way to rename. */
  const picked = $state<Record<string, number>>({});
  const identity = $derived(identities[identityIndex].identity);
  /** The colours picked for an identity, where they were. */
  const paletteOf = (one: LastUsedIdentity): IdentityPalette | undefined => {
    const own = picked[`${one.identityNumber}`];
    return own === undefined ? undefined : IDENTITY_PALETTES[own];
  };
  const identityPaletteOverride = $derived(paletteOf(identity));
  /** Where the identity's colours stand in IDENTITY_PALETTES, and a pick. */
  const colourIndex = $derived(
    picked[`${identity.identityNumber}`] ??
      identityPaletteIndex(identity.identityNumber),
  );
  const pickColour = (paletteIndex: number) => {
    picked[`${identity.identityNumber}`] = paletteIndex;
  };
  /** The phone header's colour row hangs from its avatar while it's open. */
  let headColourAnchor = $state<HTMLElement>();
  /** The colours the identity shows: picked, or the ones its number gives. */
  const currentPalette = $derived(
    identityPaletteOverride ?? identityPalette(identity.identityNumber),
  );
  const identityName = $derived(identity.name ?? `${identity.identityNumber}`);
  const signedInWith = $derived(identities[identityIndex].signedInWith);
  const current = $derived(states[identityIndex]);

  // --- Loading: the identity's data comes after the page (loading.svelte.ts) ---

  /** How long it takes, picked in the concept menu; `?load=slow` in the
   *  address picks it too, so a link can show it. */
  let loadSpeed = $state<LoadSpeed>(
    loadSpeedOf(
      new URLSearchParams(location.search).get("load") ??
        localStorage.getItem(LOAD_SPEED_KEY),
    ),
  );
  /** The account: how the identity signs in, how it would get back in, where
   *  it's signed in. The panel's cards tell it; until it lands they have
   *  their icon and title, and nothing worked out from it shows (a dot, an
   *  incident, the island, the dock). */
  const accountLoad = new Load();
  /** Its apps, which the grid shows. */
  const appsLoad = new Load();
  const known = $derived(accountLoad.state === "ready");
  /** Counts the visits: each loads the data again, and the page's blocks
   *  arrive again (`arriveIn`). */
  let visit = $state(0);

  /** The app whose details the panel shows: a tile's "⋯" opens them beside
   *  the grid (in the sheet on a phone); the tile itself opens the app. */
  let appOpen = $state<string>();
  const openedApp = $derived(
    current.apps.find((app) => app.origin === appOpen),
  );
  /** Whether the panel was open before the app's details took it, so closing
   *  them leaves it as it was. */
  let panelBeforeApp = false;

  /** Said on the home page, over the apps, by the time of day. */
  const greeting = (() => {
    const hour = new Date().getHours();
    return hour < 5
      ? "Good night"
      : hour < 12
        ? "Good morning"
        : hour < 18
          ? "Good afternoon"
          : "Good evening";
  })();

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
    }),
  );
  /** The cards as far as the page can tell: none of their tones until the
   *  account has landed, so nothing is said ahead of it. */
  const told = $derived(known ? cards : []);
  const lead = $derived(leadOf(told));
  /** A risk, told as one story the user answers; it speaks for its cards. */
  const incident = $derived(
    known
      ? incidentOf({
          scenario: current.scenario,
          signedOut: new Set(current.signedOut),
        })
      : undefined,
  );
  const coveredByIncident = (kind: CardKind) =>
    incident?.kinds.includes(kind) === true;
  /** A phone's incident sheet was put away unanswered on this visit; the next
   *  visit brings it back. */
  let incidentDismissed = $state(false);
  /** The account's rows, each its own card: how you get in, then where the
   *  identity is signed in. Apps are managed from the grid itself
   *  (Manage, by All apps' heading), so the account doesn't list them too. */
  const ACCOUNT_KINDS: CardKind[] = ["access", "recovery", "devices"];
  const accountCards = $derived(
    ACCOUNT_KINDS.map((kind) =>
      cards.find((card) => card.kind === kind),
    ).filter((card) => card !== undefined),
  );
  /** The identities to switch to: every one on the device but this. */
  const otherIdentities = $derived(
    identities
      .map((entry) => entry.identity)
      .filter((one) => one.identityNumber !== identity.identityNumber),
  );
  /** The sections' glyphs, as their cards have them. */
  const SECTION_ICONS: Record<CardKind, Component> = {
    access: KeyRoundIcon,
    recovery: BriefcaseMedicalIcon,
    devices: MonitorSmartphoneIcon,
    apps: Grid2x2CheckIcon,
    integrations: BotIcon,
  };
  /** The dot a section's icon carries on a phone, for how loud it is. */
  const dotOf = (tone: Tone): "risk" | "action" | "news" | undefined =>
    tone === "risk"
      ? "risk"
      : tone === "warning"
        ? "action"
        : tone === "info"
          ? "news"
          : undefined;
  /** What the calm island mentions: something to know, never where a card
   *  merely stands. */
  const note = $derived(told.find(({ tone }) => tone === "info"));

  // --- Apps ---

  // One grid, in the order apps were added (or arranged): a newly used app
  // goes to the end, so nothing moves by itself.
  /** The tile after the last app, which adds one. It stays while apps are
   *  arranged, as the first free cell, so the grid keeps its rows. */
  const ADD_TILE = { origin: "add-app" } as const;
  type GridItem = ConceptApp | typeof ADD_TILE;
  const gridItems = $derived<GridItem[]>([...current.apps, ADD_TILE]);
  /** How the Add tile's plus and label leave and come back: a fade only. */
  const ADD_FADE = "transition-opacity duration-150 ease-out";
  const featured = $derived(
    featuredApps.filter(
      ({ origin }) => !current.apps.some((app) => app.origin === origin),
    ),
  );

  // A page scrolls on its own, up under the fixed header (its height is
  // --header-h). On a phone the grid starts at the top like a home screen; on
  // desktop it sits in the middle of the free space (md:my-auto inside), and
  // the room kept for the island below lifts it a little above center.
  // Its scrollbar is thin, on a clear track, with room kept for it on both
  // sides whether the page scrolls or not, so every page centres its content
  // on the same line. (Overlay scrollbars take no room.) The thumb shows only
  // while the page rests in the pager (SETTLED): mid-swipe it would ride the
  // seam between two pages across the canvas.
  // On a phone a page keeps to the edge (--ii-edge) less the 10px a tile's
  // plate reaches past its logo, so the logos stand on the edge and the
  // pages' own content steps in by those 10px to stand there too.
  // A page scrolls only up and down, and sideways is the pager's: a tile's
  // count or ⋯ reaches a couple of px into the page's padding, which made
  // My apps that much wider than the screen, so a vertical swipe drifted it
  // sideways as well.
  const PAGE =
    "flex w-full shrink-0 snap-start snap-always flex-col overflow-x-hidden overflow-y-auto overscroll-y-contain px-2 @max-lg/canvas:px-[calc(var(--ii-edge)-0.625rem)] pt-[calc(var(--header-h)+0.75rem)] pb-32 scroll-pt-[calc(var(--header-h)+0.75rem)] [scrollbar-color:transparent_transparent] [scrollbar-gutter:stable_both-edges] [scrollbar-width:thin] sm:px-6";
  /** A page at rest: its thumb fades in. It hides at once when a swipe
   *  starts, as the transition is only here. */
  const SETTLED =
    "[scrollbar-color:var(--color-fg-quaternary)_transparent] transition-[scrollbar-color] duration-200 ease-out motion-reduce:transition-none";
  /** The header's height: the pages run up under it. */
  let headerHeight = $state(64);

  // Columns follow the canvas the way Apple's home screens do: four on a
  // phone, five on a tablet, seven on a desktop (the Mac's Applications
  // grid). Recent and All apps share them, so they line up tile for tile.
  // On a phone the four spread across the canvas, the first logo on the
  // start edge and the last on the end one; a narrow phone narrows them.
  const GRID =
    "grid grid-cols-[repeat(4,minmax(0,5.25rem))] justify-between gap-x-1 gap-y-1 @lg:grid-cols-[repeat(4,7rem)] @lg:gap-x-2 @2xl:grid-cols-[repeat(5,7rem)] @4xl:grid-cols-[repeat(7,var(--ii-tile))]";
  // A heading starts at the edge of the first logo, like a shelf title on
  // iOS: one line down the canvas. The 4rem logo sits centered in its
  // column, so the inset is half of what the column leaves: (7rem − 4rem) /
  // 2. On a phone the head is as wide as the logos reach, so it needs none.
  const INSET = "@lg:ps-[calc((var(--ii-tile)-var(--ii-logo))/2)]";
  /** How far the logos reach: the grid less the 1.5rem each end column
   *  leaves beside its logo (INSET). Explore and Connect keep to it, so they
   *  start on the line the greeting and the logos share, and end where the
   *  last logo does. On a phone the logos stand on the canvas's edges, and
   *  so do the pages' contents. */
  const LOGO_SPAN =
    "w-full @lg:w-[26.5rem] @2xl:w-[34rem] @4xl:w-[calc(6*var(--ii-tile)+3rem+var(--ii-logo))] max-w-full";
  /** On a wide canvas, from where the tiles start to grow, the header keeps
   *  to the logos' reach too (LOGO_SPAN), so its controls stand over the
   *  apps rather than out on the window's edges (Thomas): id.ai's glyph over
   *  the first logo, search's pill ending over the last, the tabs between
   *  them on the canvas's middle. Nothing grows for it. Narrower, the
   *  header keeps its 20px from the canvas's edges. */
  const HEADER_SPAN =
    "@[72rem]/canvas:px-[calc((100%-(6*var(--ii-tile)+3rem+var(--ii-logo)))/2)]";
  // --- Layout: the concept compares three ---

  type Layout = "panel" | "island" | "top";
  // The panel beside the canvas is the concept's main layout: on desktop it
  // stays open, so the identity's state is always in view and there's no
  // closed state to signal through. The other two stay in the concept menu
  // to compare against.
  const LAYOUTS: { value: Layout; label: string; icon: Component }[] = [
    { value: "panel", label: "Panel on the side", icon: PanelRightIcon },
    {
      value: "top",
      label: "Account on top, widgets below",
      icon: PanelTopIcon,
    },
    { value: "island", label: "Island at the bottom", icon: PanelBottomIcon },
  ];
  // A new key, so a layout picked before the panel became the main one
  // doesn't stand in for it.
  const LAYOUT_KEY = "ii-concept-layout-v3";
  // `?layout=island` in the address picks a layout too, so a link can show one.
  const readLayout = (): Layout => {
    const asked =
      new URLSearchParams(location.search).get("layout") ??
      localStorage.getItem(LAYOUT_KEY);
    return LAYOUTS.find(({ value }) => value === asked)?.value ?? "panel";
  };
  let layout = $state<Layout>(readLayout());
  const isDesktop = new MediaQuery("min-width: 768px");
  /** A phone in the main layout: the panel is the head of the canvas, above
   *  the tabs, so the identity's state is there without a tap. */
  const phoneHead = $derived(!isDesktop.current && layout === "panel");
  /** Over the grid, Explore and Connect open with their title, under the
   *  header (where the tabs are), as far down as the greeting stood when
   *  it was the header's: 8vh, between 16 and 80px, from the header's row.
   *  The page's own 12px are part of it. A phone in the main layout has
   *  its head in the page instead (pageHead). */
  const PAGE_TOP =
    "h-0 shrink-0 md:h-[clamp(0.25rem,calc(8vh-0.75rem),4.25rem)]";
  /** My apps' greeting stands a third of the way down the canvas (Thomas:
   *  the apps and their title in the middle, the controls on the edges),
   *  a little above the middle, where the eye puts it. It stays there
   *  whatever is under it, so a new row doesn't move it, until the rows
   *  would no longer fit: then the room over it gives way, down to
   *  PAGE_TOP's, and only then the page scrolls. Edit's card never moves
   *  it: while apps are arranged the room is held (mineTopHeld) and the
   *  card pushes the grid down. The canvas is the window less the page's
   *  8px above and below. */
  const MINE_TOP =
    "h-0 shrink-0 md:h-auto md:min-h-[clamp(0.25rem,calc(8vh-0.75rem),4.25rem)] md:shrink md:basis-[calc((100dvh-1rem)/3-var(--header-h)-0.75rem)]";
  /** The greeting's row, over a page: the room under it grows with the
   *  greeting. On a narrow canvas it steps in by the 10px the page keeps
   *  short of the edge, where the logos stand. */
  const HEAD_ROW =
    "shrink-0 pb-5 md:pb-10 @6xl/canvas:pb-11 @[88rem]/canvas:pb-12 @max-lg/canvas:px-2.5";
  /** A page's title over the grid, the greeting or Explore's and Connect's:
   *  it grows with the grid, 28px from seven columns, then with the tiles. */
  const PAGE_TITLE =
    "text-xl md:text-2xl md:leading-8 @4xl/canvas:text-[1.75rem] @4xl/canvas:leading-9 @6xl/canvas:text-[2rem] @6xl/canvas:leading-10 @[88rem]/canvas:text-[2.25rem] @[88rem]/canvas:leading-11";
  /** Wide enough that the panel beside the canvas still leaves the apps their
   *  room, so news may open it on arrival. Narrower, only a risk does. */
  const roomy = new MediaQuery("min-width: 1024px");
  /** The panel the island or the account opens on desktop, and the sheet on
   *  a phone. */
  let panelOpen = $state(false);
  const panelVisible = $derived(
    isDesktop.current ? layout === "panel" || panelOpen : panelOpen,
  );
  const setLayout = (next: Layout) => {
    layout = next;
    localStorage.setItem(LAYOUT_KEY, layout);
    panelOpen = false;
  };

  /** The concept's one menu (layout and scenario), and where it was opened:
   *  the corner on desktop opens it upwards, the header downwards. */
  let conceptMenu = $state<{ anchor: HTMLElement; compact: boolean }>();
  /** Concept only: every card state on one board, for review. Open from
   *  the concept menu, or with `?board=states` in the address. */
  let board = $state(
    new URLSearchParams(location.search).get("board") === "states",
  );
  /** The page's one search: the header's glyph, ⌘K and Add app open it. */
  let search = $state<ReturnType<typeof SearchOverlay>>();
  /** A phone's header controls. The menu hangs from this pill's end, at the
   *  screen's edge: from the flask, mid-pill, it would run off the start. */
  let controlsPill = $state<HTMLElement>();

  // --- Panel ---

  const TITLES: Record<CardKind, string> = {
    access: "Access",
    devices: "Devices",
    recovery: "Recovery",
    apps: "Manage apps",
    integrations: "Integrations",
  };

  let section = $state<Section>();
  let managing = $state(false);
  let selected = $state<string[]>([]);
  /** Whether the overview lists the identities to switch to. */
  let switching = $state(false);
  /** The desktop panel, which scrolls. */
  let aside = $state<HTMLElement>();

  const selectedApps = $derived(
    selected
      .map((origin) => current.apps.find((app) => app.origin === origin))
      .filter((app) => app !== undefined),
  );

  /** On desktop the three sections open in place: the card grows into its
   *  section under the identity, and the other two cards step away. */
  const grown = $derived(
    isDesktop.current &&
      section !== undefined &&
      ACCOUNT_KINDS.includes(section)
      ? section
      : undefined,
  );
  /** A section as a view of its own: in a phone's sheet, and integrations. */
  const sectionPage = $derived(grown === undefined ? section : undefined);

  // Which view the panel shows, and which way it moved to get there.
  const viewKey = $derived(
    managing
      ? selectedApps.length === 1
        ? "app"
        : selectedApps.length > 1
          ? "bulk"
          : "manage"
      : openedApp !== undefined
        ? `details:${openedApp.origin}`
        : (sectionPage ?? "overview"),
  );
  let navDirection = $state(1);
  const readingDirection = () =>
    document.documentElement.dir === "rtl" ? -1 : 1;
  const reduced = $derived(prefersReducedMotion.current);

  /** Recovery is one place: both ways back in, the email and the phrase,
   *  each with where it stands, in one dialog. A button for one way opens
   *  straight on it. */
  let recoveryOpen = $state(false);
  let recoveryFocus = $state<RecoveryMethod>();
  const openRecovery = (focus?: RecoveryMethod) => {
    recoveryFocus = focus;
    recoveryOpen = true;
  };

  /** The overview's cards, where one grows into its section on desktop. */
  let morphList = $state<HTMLElement>();
  /** While a card grows, where the others stood, so they fade there. */
  let held = $state<Record<string, Spot>>();
  /** The card with both its preview and its section while it grows or
   *  shrinks back. */
  let morphing = $state<CardKind>();
  let morphRun = 0;

  /** Opens a section, or closes the open one. On desktop, with the overview
   *  in view, its card grows into it in place and shrinks back. */
  const showSection = async (next: Section | undefined) => {
    const target = next ?? section;
    const list = morphList;
    if (
      target === undefined ||
      list === undefined ||
      !list.isConnected ||
      !isDesktop.current ||
      !panelVisible ||
      viewKey !== "overview" ||
      !ACCOUNT_KINDS.includes(target)
    ) {
      section = next;
      return;
    }
    const run = ++morphRun;
    const opening = next !== undefined;
    await morph({
      list,
      id: target,
      opening,
      reduced,
      change: async () => {
        held = opening ? spotsOf(list) : undefined;
        morphing = target;
        section = next;
        await tick();
      },
      // The grown card starts under the identity, at the top of the panel,
      // so the panel goes back there.
      settle: () => {
        if (opening && aside !== undefined) {
          aside.scrollTop = 0;
        }
      },
    });
    if (run === morphRun) {
      morphing = undefined;
      held = undefined;
    }
  };

  /** Where an item of the overview is held while a card grows over it. */
  const heldAt = (id: string): Spot | undefined =>
    grown !== undefined && grown !== id ? held?.[id] : undefined;

  const openCard = (kind: CardKind) => {
    navDirection = 1;
    switching = false;
    if (kind === "apps") {
      startManaging();
    } else if (kind === "recovery" && !isDesktop.current) {
      // A phone has recovery's own sheet for it: both ways back in, each a
      // tap from its steps, under its title and Close.
      openRecovery();
    } else {
      void showSection(kind);
    }
  };

  const closeSection = () => {
    navDirection = -1;
    if (isDesktop.current) {
      void showSection(undefined);
      return;
    }
    // A phone's sheet holds only the section, so Close puts it away.
    section = undefined;
    panelOpen = false;
  };

  /** A section from the phone's header: in the sheet, over the apps. */
  const openFromHead = (kind: CardKind) => {
    openCard(kind);
    if (kind !== "recovery") {
      panelOpen = true;
    }
  };

  /** The room over My apps' greeting (MINE_TOP), and its height held while
   *  apps are arranged: Edit's card opens under the greeting and pushes the
   *  grid down, and the greeting and Done stay where Edit was, so one spot
   *  turns Edit on and off. Without the hold, a card that no longer fits
   *  takes its room from over the greeting and lifts it. */
  let mineTop = $state<HTMLElement>();
  let mineTopHeld = $state<number>();
  let mineTopRelease: ReturnType<typeof setTimeout> | undefined;

  const startManaging = () => {
    clearTimeout(mineTopRelease);
    if (mineTop !== undefined && mineTopHeld === undefined) {
      mineTopHeld = mineTop.getBoundingClientRect().height;
    }
    navDirection = 1;
    appOpen = undefined;
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
    // The hold lets go once the card's room has closed (250ms), so Done
    // doesn't move the greeting either.
    clearTimeout(mineTopRelease);
    mineTopRelease = setTimeout(
      () => (mineTopHeld = undefined),
      reduced ? 0 : 300,
    );
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

  /** A tile's "⋯": its details take the panel, or the sheet. */
  const openAppDetails = (app: ConceptApp) => {
    if (managing) return;
    if (appOpen === undefined) {
      panelBeforeApp = panelOpen;
    }
    navDirection = 1;
    section = undefined;
    switching = false;
    appOpen = app.origin;
    panelOpen = true;
  };

  const closeAppDetails = () => {
    navDirection = -1;
    appOpen = undefined;
    if (!panelBeforeApp || !isDesktop.current) {
      panelOpen = false;
    }
  };

  const islandView = $derived<IslandView | undefined>(
    managing
      ? panelVisible
        ? undefined
        : "select"
      : layout === "island" && !panelVisible && known
        ? lead !== undefined
          ? "lead"
          : "calm"
        : undefined,
  );

  // --- Top layout: the account in the header, what needs you below ---

  /** What earns a widget, most urgent first: a risk, the step urged, and
   *  info that offers a step (something new, a step suggested). */
  const URGENCY: Tone[] = ["risk", "warning", "info"];
  const attention = $derived(
    told
      .filter(
        ({ tone, ask }) =>
          tone === "risk" ||
          tone === "warning" ||
          (tone === "info" && ask !== undefined),
      )
      .sort((a, b) => URGENCY.indexOf(a.tone) - URGENCY.indexOf(b.tone)),
  );
  /** An incident takes one widget for all its cards, in front: it opens the
   *  panel, which leads with it. On a phone its card at the top of the apps
   *  stands in for the widget. */
  const dockItems = $derived<DockItem[]>(
    [
      ...(incident !== undefined && isDesktop.current
        ? [
            {
              kind: incident.kinds[0],
              tone: "risk" as const,
              label: incident.ask,
              primary: true,
            },
          ]
        : []),
      ...attention
        .filter((card) => !coveredByIncident(card.kind))
        // News leaves once the panel has shown it; a step stays until done.
        .filter(
          (card) =>
            card.news !== true ||
            !(seen[seenId] ?? []).includes(seenKeyOf(card)),
        )
        .map((card) => ({
          kind: card.kind,
          tone: card.tone,
          label: card.ask ?? card.sentence,
          primary: isLoud(card),
        })),
    ].slice(0, 3),
  );
  /** The closed account's dot, while something needs the user (as the panel
   *  layout's avatar on a phone has it): red for a risk, amber otherwise. */
  const accountAttention = $derived<"risk" | "action" | undefined>(
    told.some(({ tone }) => tone === "risk")
      ? "risk"
      : needsAttention(told)
        ? "action"
        : undefined,
  );
  /** The account opens on its overview, unless apps are being managed. */
  const openAccount = () => {
    if (!managing) {
      section = undefined;
    }
    appOpen = undefined;
    panelOpen = true;
  };

  const actFromDock = (kind: CardKind) => {
    if (coveredByIncident(kind)) {
      openAccount();
      return;
    }
    const card = cards.find((one) => one.kind === kind);
    if (card !== undefined) {
      runLead(card);
    }
  };

  // --- Arriving: whether the panel meets the user ---

  // On desktop the panel opens by itself only as a visit starts, never in the
  // middle of one, and only when it has something to say: on the first visit,
  // when something has come up since the panel last showed, and on every visit
  // while a risk is left unanswered. Otherwise it is as the user left it.
  // Whatever the open panel shows counts as read: once it is closed, the
  // same steps wait in the dock and don't open it again, and news is done.
  /** Whether the panel was left open, to come back to it as it was. */
  const PANEL_KEY = "ii-concept-panel-v1";
  /** What the panel last showed, per identity: only news opens it, and news
   *  it has shown leaves the dock. */
  const SEEN_KEY = "ii-concept-seen-v1";
  const seenKeyOf = ({ kind, sentence }: CardStatus) => `${kind}:${sentence}`;
  const readSeen = (): Record<string, string[]> => {
    try {
      return JSON.parse(localStorage.getItem(SEEN_KEY) ?? "{}") as Record<
        string,
        string[]
      >;
    } catch {
      return {};
    }
  };
  let seen = $state(readSeen());
  const seenId = $derived(`${identity.identityNumber}`);

  const opensOnArrival = (): boolean => {
    if (!isDesktop.current || layout === "panel") {
      return false;
    }
    if (incident !== undefined) {
      return true;
    }
    const left = localStorage.getItem(PANEL_KEY);
    const shown = seen[seenId] ?? [];
    const news =
      left === null ||
      attention.some((card) => !shown.includes(seenKeyOf(card)));
    return (news && roomy.current) || left === "open";
  };

  /** A visit starts: as the page loads, and from the concept menu. The
   *  identity's data is fetched again, each part landing on its own, and
   *  the panel decides whether to meet the user only once it knows what it
   *  would say. */
  const arrive = () => {
    incidentDismissed = false;
    section = undefined;
    appOpen = undefined;
    switching = false;
    managing = false;
    selected = [];
    visit += 1;
    const speed =
      LOAD_SPEEDS.find(({ value }) => value === loadSpeed) ?? LOAD_SPEEDS[0];
    panelOpen = false;
    accountLoad.start(speed.account, {
      onReady: () => (panelOpen = opensOnArrival()),
    });
    appsLoad.start(speed.apps, { fails: speed.appsFail });
  };
  arrive();

  /** Concept only: how long the data takes; a new visit shows it. */
  const setLoadSpeed = (speed: LoadSpeed) => {
    loadSpeed = speed;
    saveLoadSpeed(speed);
    arrive();
  };

  /** The apps that failed, asked for again. */
  const retryApps = () => {
    const normal = LOAD_SPEEDS.find(({ value }) => value === "normal");
    appsLoad.start(normal?.apps ?? 800);
  };

  // The open panel has shown what it holds now (once there is something).
  $effect(() => {
    if (!panelVisible || !known) {
      return;
    }
    const keys = attention.map(seenKeyOf);
    const shown = untrack(() => seen[seenId]) ?? [];
    if (JSON.stringify(shown) !== JSON.stringify(keys)) {
      seen[seenId] = keys;
      localStorage.setItem(SEEN_KEY, JSON.stringify(untrack(() => seen)));
    }
  });

  // How the desktop panel was left. A phone's sheet is closed on every visit.
  $effect(() => {
    if (isDesktop.current && layout !== "panel") {
      localStorage.setItem(PANEL_KEY, panelOpen ? "open" : "closed");
    }
  });

  /** Concept only: the next visit is the first again. */
  const arriveFirst = () => {
    localStorage.removeItem(PANEL_KEY);
    localStorage.removeItem(SEEN_KEY);
    seen = {};
    arrive();
  };

  // --- Apps grid pages ---

  const PAGES = [
    { id: "mine", label: "My apps" },
    { id: "explore", label: "Explore" },
    { id: "connect", label: "Connect" },
  ] as const;
  /** Explore and Connect open with their title and, under it, a line on
   *  what they're for, where My apps has the greeting: the tabs are in the
   *  header now, so the greeting would only say the same again on every
   *  page. The same on a phone and over the grid. */
  const PAGE_INTROS: ({ title: string; line: string } | undefined)[] = [
    undefined,
    {
      title: "Explore apps",
      line: "Apps you can sign in to with Internet Identity.",
    },
    {
      title: "Use your apps from AI",
      line: "Your assistant signs in to your apps as you, with only the access you allow.",
    },
  ];
  let activePage = $state(0);
  let pager = $state<HTMLElement>();
  const pageRefs = $state<HTMLElement[]>([]);
  const tabRefs = $state<HTMLElement[]>([]);
  /** Each tab's width, for the thumb under the selected one. The tabs sit
   *  edge to edge after the pill's padding, so a tab starts where the ones
   *  before it end; `start` is from the first tab, the padding added where
   *  the pill is drawn. */
  const tabWidths = $state(PAGES.map(() => 0));
  const thumb = $derived(
    tabWidths.every((width) => width > 0)
      ? {
          start: tabWidths.slice(0, activePage).reduce((a, b) => a + b, 0),
          width: tabWidths[activePage],
        }
      : undefined,
  );

  // The tabs stand in the header, centred over the apps, and stay there as
  // a page scrolls under them, as a phone's tab bar stays at the bottom.
  // Each page opens with the greeting or its title, which scrolls away with it.
  /** Each page's scroll, which moves the head while that page is shown. */
  const pageScroll = $state(PAGES.map(() => 0));
  /** The header, and each page's greeting or title row over the grid: the
   *  header lifts once that row has gone up under it. */
  let headerElement = $state<HTMLElement>();
  const pageHeads = $state<HTMLElement[]>([]);
  // On a phone the head (the greeting and the cards) is the top of each page
  // instead, and the tabs are the tab bar at the bottom: a touch anywhere on
  // the head scrolls the page, as on any phone screen, and the cards go up
  // under the logo whole. Each page's copy, measured.
  const pageHeadHeights = $state(PAGES.map(() => 0));
  /** How far the head goes before it's out of sight. */
  const headHeight = $derived(
    phoneHead ? (pageHeadHeights[activePage] ?? 0) : 0,
  );
  const headLift = $derived(Math.min(pageScroll[activePage] ?? 0, headHeight));
  /** The head has gone up out of sight. */
  const headCollapsed = $derived(headHeight > 0 && headLift >= headHeight - 2);
  // The header's controls are pills in the canvas's own colour, so at rest
  // they don't show. Once a page's content runs under them they lift off
  // the canvas, as the dock's pills sit over it, and keep the header readable
  // over whatever runs under it. Over the grid that's once the greeting has
  // gone up under the header; on a phone, past the 0.75rem PAGE keeps under
  // it. Read again as the page scrolls.
  const headerLifted = $derived.by(() => {
    const scrolled = pageScroll[activePage] ?? 0;
    if (phoneHead) return scrolled > 12;
    const head = pageHeads[activePage];
    if (scrolled === 0 || head === undefined || headerElement === undefined) {
      return false;
    }
    return (
      head.getBoundingClientRect().top <
      headerElement.getBoundingClientRect().bottom
    );
  });
  /** The pages scroll on their own; the pager hears them while capturing. */
  const onPageScroll = (event: Event) => {
    const index = pageRefs.indexOf(event.target as HTMLElement);
    if (index !== -1) {
      pageScroll[index] = pageRefs[index].scrollTop;
    }
  };
  const pill = $derived([
    "pointer-events-auto rounded-full border border-transparent",
    headerLifted ? "ii-elevation-6 bg-(--ii-floating)" : "bg-(--ii-canvas)",
  ]);
  const PILL_MOTION =
    "transition-[background-color,box-shadow] duration-200 ease-out motion-reduce:transition-none";

  /** The page a tab sent the pager to, while it's on its way. A page it only
   *  passes (Explore, from My apps to Connect) doesn't take the tab for a
   *  moment, or the thumb would run there and back. */
  let headingTo: number | undefined;
  /** The page the pager shows, or is nearest to mid-swipe. */
  const pageInView = () =>
    pager === undefined
      ? activePage
      : Math.round(Math.abs(pager.scrollLeft) / pager.clientWidth);

  // A tab doesn't carry the pages across the canvas, past any between: the
  // page in view steps aside and fades, the pager moves under it unseen, and
  // the tab's own page arrives with a short shift from its side (pageLeave,
  // pageArrive). A swipe still moves them whole, under the finger.
  /** A tab's switch while its old page leaves: another tab picked meanwhile
   *  only changes where it lands. */
  let tabSwitch: { from: number; to: number } | undefined;
  /** The switch's animations, to cut short. */
  let pageMotion: Animation[] = [];

  const goToPage = (index: number) => {
    activePage = index;
    if (tabSwitch !== undefined) {
      tabSwitch.to = index;
      headingTo = index;
      return;
    }
    const from = pageInView();
    const leaving = pageRefs[from];
    if (index === from || leaving === undefined) {
      // On it already, or mid-swipe onto it: it settles where it is.
      pageRefs[index]?.scrollIntoView({
        behavior: reduced ? "instant" : "smooth",
        block: "nearest",
        inline: "start",
      });
      return;
    }
    headingTo = index;
    tabSwitch = { from, to: index };
    // An arrival still under way leaves from where it got to: the leave
    // starts from what's on screen, so the arrival can go at once.
    const leave = pageLeave(leaving.children, {
      direction: Math.sign(index - from) * readingDirection(),
      reduced,
    });
    for (const animation of pageMotion) animation.cancel();
    pageMotion = leave;
    void Promise.all(leave.map((animation) => animation.finished)).then(
      landPage,
      () => {},
    );
  };

  /** The old page is out of sight: the pager jumps to the tab's page, and it
   *  comes in. */
  const landPage = () => {
    if (tabSwitch === undefined) return;
    const { from, to } = tabSwitch;
    tabSwitch = undefined;
    const page = pageRefs[to];
    if (to === from) {
      // Picked again before it was gone: the pager never moved.
      headingTo = undefined;
    } else {
      page?.scrollIntoView({
        behavior: "instant",
        block: "nearest",
        inline: "start",
      });
    }
    const held = pageMotion;
    pageMotion =
      page === undefined
        ? []
        : pageArrive(page.children, {
            direction: Math.sign(to - from) * readingDirection(),
            reduced,
          });
    // Let go only now, in the same frame: the old page is out of view, or
    // (picked again) under its own arrival.
    for (const animation of held) animation.cancel();
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
          if (!entry.isIntersecting) continue;
          const index = pageRefs.indexOf(entry.target as HTMLElement);
          if (headingTo !== undefined && index !== headingTo) continue;
          // Where scrollend isn't fired, arriving is what ends the trip.
          headingTo = undefined;
          activePage = index;
        }
      },
      { root: pager, threshold: 0.6 },
    );
    for (const page of pageRefs) {
      observer.observe(page);
    }
    return () => observer.disconnect();
  });

  /** The pager stopped: if a swipe cut the trip short, the tab is the page
   *  it stopped on. */
  const onPagerScrollEnd = () => {
    if (headingTo === undefined) return;
    headingTo = undefined;
    activePage = pageInView();
  };

  /** How much of a page is in view once it rests: all of it, give or take
   *  the subpixels of a fractional width. */
  const SETTLED_AT = 0.99;
  /** Which pages rest in the pager, so they show their scrollbar (SETTLED).
   *  Mid-swipe neither page does. */
  const settled = $state(PAGES.map(() => false));
  $effect(() => {
    if (pager === undefined) return;
    const observer = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          const index = pageRefs.indexOf(entry.target as HTMLElement);
          settled[index] = entry.intersectionRatio >= SETTLED_AT;
        }
      },
      { root: pager, threshold: SETTLED_AT },
    );
    for (const page of pageRefs) {
      observer.observe(page);
    }
    return () => observer.disconnect();
  });

  // --- Actions on the mock ---

  const showStartPage = () =>
    toaster.create({
      title: "Make id.ai your start page",
      description:
        "In your browser's settings, under “On startup”, open id.ai.",
      duration: 5000,
    });

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

  /** Either answer settles the incident; "it wasn't me" leaves the next step
   *  (a new recovery phrase) to the cards. */
  const answerIncident = ({ patch, message }: IncidentAnswer) => {
    resolve(patch, message);
  };

  /** The loud card's button: finishes the step where the concept can, otherwise
   *  opens the card's section. */
  const runLead = (card: CardStatus) => {
    if (card.kind === "recovery") {
      openRecovery();
      return;
    }
    const action = leadActionOf(card, current.scenario);
    if (action.resolve !== undefined) {
      resolve(action.resolve.patch, action.resolve.message);
      return;
    }
    openCard(card.kind);
    panelOpen = true;
  };

  /** A preset stands for the state the user arrives to, so it starts a visit. */
  const applyPreset = (index: number) => {
    current.scenario = { ...scenarioPresets[index].scenario };
    current.signedOut = [];
    navDirection = -1;
    arrive();
  };

  const signOut = (id: string) => {
    current.signedOut = [...current.signedOut, id];
    toaster.success({ title: "Signed out", duration: 2500 });
  };

  const readAll = (app: ConceptApp) => {
    app.unread = [];
  };

  /** Opening an app makes it the most recent. The app opens in a new tab, so
   *  Recent reorders once this tab is back in view, where the move is seen; a
   *  tab opened in the background (⌘-click) never hides this one, so the row
   *  moves right away. */
  const markUsed = (app: ConceptApp) => {
    // With history off, nothing records where the identity signs in.
    if (!current.keepHistory) return;
    const bump = () => {
      app.lastUsedMillis = Date.now();
    };
    setTimeout(() => {
      if (document.visibilityState === "visible") {
        bump();
      } else {
        document.addEventListener("visibilitychange", bump, { once: true });
      }
    }, 300);
  };

  const read = (app: ConceptApp, id: string) => {
    app.unread = app.unread.filter((notification) => notification.id !== id);
  };

  const setNotifications = (apps: ConceptApp[], allowed: boolean) => {
    for (const app of apps) {
      app.notificationsAllowed = allowed;
    }
  };

  /** The tile's menu and the selection bar change a setting the grid shows
   *  only for an app that has notified, so they say so. */
  const setNotificationsAndSay = (apps: ConceptApp[], allowed: boolean) => {
    setNotifications(apps, allowed);
    toaster.success({
      title: `Notifications ${allowed ? "on" : "off"} for ${apps.length === 1 ? apps[0].name : `${apps.length} apps`}`,
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
    if (appOpen !== undefined && origins.includes(appOpen)) {
      closeAppDetails();
    }
    toaster.create({
      title:
        apps.length === 1
          ? `${apps[0].name} removed`
          : `${apps.length} apps removed`,
      description: "You're signed out. You can find them again in Explore.",
      closable: true,
      duration: 5000,
      action: { label: "Undo", onClick: () => (state.apps = before) },
    });
  };

  /** Removing asks first, and says what it does: sign out, no more
   *  notifications, and the app is still in Explore. */
  let removing = $state<ConceptApp[]>();
  const askRemove = (apps: ConceptApp[]) => {
    removing = apps;
  };

  /** From Explore or the add popover: the app joins the end of the
   *  grid, as a newly installed app does on a phone. */
  const addApp = (app: FeaturedApp, usedMillis?: number) => {
    if (current.apps.some(({ origin }) => origin === app.origin)) return;
    current.apps = [
      ...current.apps,
      {
        origin: app.origin,
        name: app.name,
        logo: app.logo,
        url: app.url,
        oneLiner: app.oneLiner,
        lastUsedMillis: usedMillis ?? Date.now(),
        notificationsAllowed: false,
        lastBrowser: "Chrome on Mac",
        unread: [],
      },
    ];
    current.recentOnly = current.recentOnly.filter(
      (entry) => entry.app.origin !== app.origin,
    );
    toaster.success({ title: `${app.name} added to My apps`, duration: 2500 });
  };
  const isAdded = (origin: string) =>
    current.apps.some((app) => app.origin === origin);
  /** An app opened from the search counts as used, as a tap on its tile. */
  const openedFromSearch = ({ origin }: FeaturedApp) => {
    const app = current.apps.find((one) => one.origin === origin);
    if (app !== undefined) {
      readAll(app);
      markUsed(app);
    }
  };
  /** A site that isn't in the curated list, added by its address. */
  const addLink = (host: string) =>
    addApp({
      origin: `https://${host}`,
      name: host,
      logo: "",
      url: `https://${host}`,
    });

  const switchIdentity = (identityNumber: bigint) => {
    identityIndex = identities.findIndex(
      (entry) => entry.identity.identityNumber === identityNumber,
    );
    switching = false;
    section = undefined;
    appOpen = undefined;
    stopManaging();
  };

  // A panel that opens again starts with the list folded, on its overview.
  $effect(() => {
    if (!panelVisible) {
      appOpen = undefined;
      switching = false;
      section = undefined;
    }
  });

  /** Light or dark, to try the concept in both; the panel's foot switches it. */
  const theme = new ConceptTheme();
  const THEMES = [
    { value: "light", label: "Light", icon: SunIcon },
    { value: "dark", label: "Dark", icon: MoonIcon },
  ] as const;

  // The canvas header's corner holds the account while the panel is closed
  // and Close while it is open, so focus follows from one to the other: the
  // control pressed leaves the page, and focus would drop to the body.
  let accountButton = $state<HTMLButtonElement>();
  let closeButton = $state<HTMLButtonElement>();
  let wasVisible = untrack(() => panelVisible);
  $effect(() => {
    const visible = panelVisible;
    if (visible === wasVisible) return;
    wasVisible = visible;
    void tick().then(() => {
      const active = document.activeElement;
      if (visible) {
        if (active === null || active === document.body) {
          closeButton?.focus({ preventScroll: true });
        }
      } else if (
        active === null ||
        active === document.body ||
        // The panel goes inert as it closes; focus inside it comes back.
        aside?.contains(active) === true
      ) {
        accountButton?.focus({ preventScroll: true });
      }
    });
  });

  const onWindowKeydown = (event: KeyboardEvent) => {
    if (event.key !== "Escape" || event.defaultPrevented) {
      return;
    }
    // An open popover or dialog takes Escape first.
    const overlaid =
      document.querySelector(".popover:popover-open, dialog[open]") !== null;
    if (grown !== undefined && viewKey === "overview" && !overlaid) {
      event.preventDefault();
      closeSection();
      return;
    }
    if (
      (layout === "island" || layout === "top") &&
      isDesktop.current &&
      panelOpen &&
      // An open popover takes Escape first.
      document.querySelector(".popover:popover-open") === null
    ) {
      panelOpen = false;
    }
  };
</script>

<svelte:window onkeydown={onWindowKeydown} />

{#snippet headCards()}
  <!-- Phone: the panel's three sections as the head's own cards, side by
       side, so how you get in, how you'd get back in and where you're signed
       in are there as the page opens. Each opens its section over the apps. -->
  <div class="pointer-events-auto grid grid-cols-3 gap-1.5 pb-8">
    {#each accountCards as card (card.kind)}
      <HeadCard
        {card}
        title={TITLES[card.kind]}
        icon={SECTION_ICONS[card.kind]}
        loading={!known}
        onOpen={() => openFromHead(card.kind)}
      />
    {/each}
  </div>
{/snippet}

{#snippet greetingLine(inPage: boolean)}
  <!-- On a phone, at the top of the page, it's a step smaller, so a long name
       keeps to the line, and closer to the logo than to the cards under it.
       Over the grid it's the page's title: it grows with the grid (28px from
       seven columns, then with the tiles), and its row (HEAD_ROW) keeps it
       well apart from what's under it. -->
  <p
    class={[
      "text-text-primary flex min-w-0 items-center gap-x-2 leading-7 font-semibold tracking-tight",
      inPage ? ["pb-6 text-lg", INSET] : PAGE_TITLE,
    ]}
  >
    <span class="shrink-0">{greeting},</span>
    <span
      aria-hidden="true"
      class="size-2.5 shrink-0 rounded-full outline-1 -outline-offset-1 outline-black/8 transition-colors duration-200 ease-out motion-reduce:transition-none dark:outline-white/12"
      style:background-color={currentPalette.background}
    ></span>
    <span class="min-w-0 truncate">{identityName}</span>
  </p>
{/snippet}

{#snippet pageIntro(index: number, inPage: boolean)}
  <!-- Explore's and Connect's title, where My apps has the greeting, and as
       large, so a tab changes the words and not the layout; under it, one
       line on what the page is for, quieter. On a phone a step smaller, as
       the greeting is there. -->
  {@const intro = PAGE_INTROS[index]}
  {#if intro !== undefined}
    <hgroup
      class={[
        "flex min-w-0 flex-col",
        inPage ? "gap-1" : "gap-1.5 @6xl/canvas:gap-2",
      ]}
    >
      <h2
        class={[
          "text-text-primary leading-7 font-semibold tracking-tight text-balance",
          inPage ? "text-lg" : PAGE_TITLE,
        ]}
      >
        {intro.title}
      </h2>
      <p
        class={[
          "text-text-tertiary text-pretty",
          inPage
            ? "text-sm leading-5"
            : "text-[15px] leading-6 @6xl/canvas:text-base @[88rem]/canvas:text-lg @[88rem]/canvas:leading-7",
        ]}
      >
        {intro.line}
      </p>
    </hgroup>
  {/if}
{/snippet}

{#snippet pageIntroRow(index: number)}
  <!-- Over the grid: the content keeps to the logos' span, so the title needs
       no inset. -->
  <div bind:this={pageHeads[index]} class={HEAD_ROW}>
    {@render pageIntro(index, false)}
  </div>
{/snippet}

{#snippet pageHead(index: number)}
  <!-- Phone: the greeting and the cards head My apps itself, so they scroll
       with it from wherever a touch starts. Explore and Connect open with
       their own title in the greeting's place instead, so the head isn't
       said again on every page (Thomas: the identity's access and recovery
       are what get you into these apps, so they belong to My apps). As wide
       as the grid (the page keeps 10px short of the edge, so they step in by
       those). Only the shown page's copy is live; the others are what a
       swipe brings in. Only My apps' head is measured: the section icons
       that take the cards' place in the top row once they've scrolled away
       belong to My apps too. -->
  {#if index === 0}
    <div
      bind:clientHeight={pageHeadHeights[index]}
      inert={index !== activePage}
      use:arriveIn={{ visit, reduced }}
      class="mx-2.5 shrink-0 @lg:mx-auto @lg:w-[29.5rem] @lg:max-w-full @2xl:w-[37rem]"
    >
      {@render greetingLine(true)}
      {@render headCards()}
    </div>
  {:else}
    <div
      inert={index !== activePage}
      class={[
        "mx-2.5 shrink-0 pb-8 @lg:mx-auto @lg:w-[29.5rem] @lg:max-w-full @2xl:w-[37rem]",
        INSET,
      ]}
    >
      {@render pageIntro(index, true)}
    </div>
  {/if}
{/snippet}

{#snippet pageTabs(atBottom: boolean)}
  <!-- The pages. Over the grid they're a segmented control in the header,
       centred over the apps: a track in the shelf's grey, the colour the
       canvas groups things in, solid so it stays whole when content runs
       under it and the pill lifts (see `pill`). On a phone they're the tab
       bar, floating over the blur at the bottom, always lifted since content
       always runs under it, and a step larger for a thumb. Either way they
       stay put however far a page scrolls. -->
  {@const padding = atBottom ? 4 : 3}
  <div
    role="tablist"
    aria-label="Apps"
    tabindex="-1"
    onkeydown={onTabKeydown}
    class={[
      "relative flex items-center",
      atBottom
        ? "ii-elevation-6 pointer-events-auto h-12 rounded-full border border-transparent bg-(--ii-floating) p-1"
        : [
            "pointer-events-auto h-10 rounded-full border border-transparent bg-(--ii-shelf) p-[3px]",
            headerLifted && "ii-elevation-6",
            PILL_MOTION,
          ],
    ]}
  >
    <!-- The selected tab sits on a thumb, as in a segmented control, so it
         reads by its shape and not only by its darker text. In the header
         it's raised off the grey track, a surface with a small shadow, as the
         theme switch's option in use is; 3px of the track show around it, so
         the two read as one control. White in light; in dark the top level,
         since the track is already a step up from the canvas and a lower one
         would barely part from it. The phone's tab bar is already raised,
         so there the thumb is the selected overlay. It slides to a tab
         picked or swiped to. Mounted once the tabs are measured, so it
         doesn't grow in from nothing on load. -->
    {#if thumb !== undefined}
      <span
        aria-hidden="true"
        class={[
          "absolute rounded-full transition-[inset-inline-start,width] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
          atBottom
            ? "inset-y-1 bg-(--ii-selected)"
            : "inset-y-[3px] bg-(--ii-top) shadow-(--ii-shadow-3)",
        ]}
        style:inset-inline-start={`${padding + thumb.start}px`}
        style:width={`${thumb.width}px`}
      ></span>
    {/if}
    {#each PAGES as page, index (page.id)}
      <button
        bind:this={tabRefs[index]}
        bind:offsetWidth={tabWidths[index]}
        type="button"
        role="tab"
        id={`tab-${page.id}`}
        aria-selected={activePage === index}
        aria-controls={`page-${page.id}`}
        tabindex={activePage === index ? 0 : -1}
        onclick={() => goToPage(index)}
        class={[
          "focus-visible:outline-focus-ring relative rounded-full text-sm font-semibold whitespace-nowrap transition-colors duration-150 ease-out focus-visible:outline-2 motion-reduce:transition-none",
          atBottom ? "h-10 px-4" : "h-8 px-3.5",
          activePage === index
            ? "text-text-primary"
            : "text-text-tertiary hover:text-text-secondary",
        ]}
      >
        {page.label}
      </button>
    {/each}
  </div>
{/snippet}

{#snippet editWords(align: string)}
  <!-- Both words share one cell, so the button keeps the wider one's width
       and doesn't shift as Edit turns into Done; the word in use fades in
       over the other. -->
  <span
    class={["grid justify-items-center *:col-start-1 *:row-start-1", align]}
  >
    <span
      aria-hidden={managing}
      class={[
        "transition-opacity duration-150 ease-out",
        managing && "opacity-0",
      ]}
    >
      Edit
    </span>
    <span
      aria-hidden={!managing}
      class={[
        "transition-opacity duration-150 ease-out",
        !managing && "opacity-0",
      ]}
    >
      Done
    </span>
  </span>
{/snippet}

{#snippet appTile(app: ConceptApp, index: number)}
  <AppTile
    name={app.name}
    logo={app.logo}
    url={app.url}
    unread={app.unread}
    mode={managing ? "select" : "open"}
    selected={selected.includes(app.origin)}
    current={appOpen === app.origin && panelVisible}
    {index}
    onToggle={() => toggleSelected(app.origin)}
    onDetails={() => openAppDetails(app)}
    onOpen={() => {
      readAll(app);
      markUsed(app);
    }}
    onClearAll={() => readAll(app)}
    notificationsAllowed={app.notificationsAllowed}
    muted={!app.notificationsAllowed && app.lastNotifiedMillis !== undefined}
    onSetNotifications={(allowed) => setNotificationsAndSay([app], allowed)}
    onManage={() => manageFrom(app)}
    onRemove={() => askRemove([app])}
  />
{/snippet}

{#snippet addTile()}
  <!-- After the last app, the way to add one by hand: the page's search,
       which also takes a link. While apps are arranged it stays
       where it is as the first free cell: the plus and the label fade, the
       dashed frame stays, and the free cells after it fill out its row. -->
  <button
    type="button"
    onclick={() => search?.show()}
    inert={managing}
    aria-haspopup="dialog"
    class="group focus-visible:ring-focus-ring flex w-full flex-col items-center gap-2.5 rounded-2xl px-1.5 pt-4 pb-3 transition-[scale] duration-160 ease-out outline-none focus-visible:ring-2 active:scale-[0.97] motion-reduce:active:scale-100"
  >
    <!-- Like an app's tile, no plate: hovered or focused, the frame grows
         by 6%. -->
    <span
      class="border-border-primary text-fg-tertiary grid size-(--ii-logo) place-items-center rounded-2xl border-[1.5px] border-dashed transition-[scale] duration-220 ease-(--ii-ease-out) group-hover:scale-106 group-focus-visible:scale-106 motion-reduce:transition-none"
    >
      <span class={["grid", ADD_FADE, managing && "opacity-0"]}>
        <PlusIcon class="size-6" aria-hidden="true" />
      </span>
    </span>
    <span
      class={[
        "text-text-tertiary text-(length:--ii-tile-label) leading-tight",
        ADD_FADE,
        managing && "opacity-0",
      ]}
    >
      Add app
    </span>
  </button>
{/snippet}

{#snippet tilesSkeleton()}
  <!-- The apps on their way: as many tiles as this device last saw for the
       identity (in the concept, as many as it has), each an app tile's size:
       the logo's plate and one line of its name, in the grid's own columns,
       so the grid lands where they stood. The product's skeleton tint. -->
  <ul class={GRID}>
    {#each { length: current.apps.length + 1 }, index (index)}
      <li class="flex flex-col items-center gap-2.5 px-1.5 pt-4 pb-3">
        <span
          class="skeleton block size-(--ii-logo) [--ii-skeleton-radius:1rem]"
        ></span>
        <span
          class="flex h-[calc(var(--ii-tile-label)*1.25)] w-full items-center justify-center"
        >
          <span class="skeleton block h-2.5 w-3/5"></span>
        </span>
      </li>
    {/each}
  </ul>
{/snippet}

{#snippet appsFailed()}
  <!-- The apps didn't come: said as calmly as "No apps yet" is, in the same
       panel, with the one way on. -->
  <div
    role="alert"
    class="border-border-tertiary mx-2.5 flex flex-col items-center gap-2 rounded-3xl border bg-(--ii-hover) px-6 py-9 text-center @lg:mx-0 @lg:w-[min(36rem,calc(100vw-2rem))]"
  >
    <p class="text-text-primary text-base font-semibold">
      Your apps didn't load
    </p>
    <p class="text-text-tertiary max-w-xs text-sm text-pretty">
      Check your connection and try again. Signing in to them still works.
    </p>
    <button
      type="button"
      onclick={retryApps}
      class="btn btn-secondary btn-sm mt-3 rounded-full"
    >
      Try again
    </button>
  </div>
{/snippet}

{#snippet firstApps()}
  <!-- No apps yet: a calm panel that says what will be here, with the one
       action inside it. -->
  <div
    class="border-border-tertiary mx-2.5 flex flex-col items-center gap-2 rounded-3xl border bg-(--ii-hover) px-6 py-9 text-center @lg:mx-0 @lg:w-[min(36rem,calc(100vw-2rem))]"
  >
    <p class="text-text-primary text-base font-semibold">No apps yet</p>
    <p class="text-text-tertiary max-w-xs text-sm text-pretty">
      Apps you sign in to with Internet Identity show up here. You can also add
      one yourself.
    </p>
    <button
      type="button"
      onclick={() => search?.show()}
      aria-haspopup="dialog"
      class="btn btn-primary btn-sm mt-3 rounded-full"
    >
      <PlusIcon class="size-4" aria-hidden="true" />
      Add your first app
    </button>
  </div>
  <!-- A short Explore, in rows: suggestions that are plainly not the user's
       yet, each with Add, and the way to all of them last. -->
  <section
    class="mx-2.5 mt-10 flex flex-col gap-2 @lg:mx-0 @lg:w-[min(36rem,calc(100vw-2rem))]"
  >
    <h2 class="text-text-tertiary ps-4 text-sm font-medium">
      Suggested for you
    </h2>
    <Elevated
      offset={1}
      as="ul"
      class="ii-rows flex flex-col overflow-hidden rounded-2xl"
    >
      {#each featuredApps.slice(0, 4) as app (app.origin)}
        <li>
          <AppRow {app} added={isAdded(app.origin)} onAdd={() => addApp(app)} />
        </li>
      {/each}
      <li>
        <button
          type="button"
          onclick={() => goToPage(1)}
          class="ii-hover focus-visible:ring-focus-ring text-text-primary flex w-full items-center gap-3 px-4 py-3.5 text-start text-sm font-medium outline-none focus-visible:ring-2 focus-visible:ring-inset"
        >
          <CompassIcon class="text-fg-tertiary size-4.5" aria-hidden="true" />
          <span class="flex-1">Explore all apps</span>
          <ChevronRightIcon
            class="text-fg-quaternary size-4 rtl:-scale-x-100"
            aria-hidden="true"
          />
        </button>
      </li>
    </Elevated>
  </section>
{/snippet}

{#snippet headAvatar()}
  <IdentityAvatar {identity} palette={identityPaletteOverride} size="sm" />
{/snippet}

{#snippet headIdentities(close: () => void)}
  <!-- Every identity on the device, this one checked, then the way to add
       one, as an account menu leads with its accounts. -->
  <IdentityList
    id="head-identity-list"
    {identity}
    signedInWith={signedInWith.title}
    others={otherIdentities}
    onSwitch={(identityNumber) => {
      close();
      switchIdentity(identityNumber);
    }}
    onAdd={() => {
      close();
      unavailable("Adding an identity");
    }}
    {paletteOf}
  />
  <hr class="border-border-tertiary" />
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
      (conceptMenu = {
        anchor: compact
          ? (controlsPill ?? event.currentTarget)
          : event.currentTarget,
        compact,
      })}
    class="text-fg-quaternary hover:text-fg-tertiary focus-visible:outline-focus-ring grid size-8 shrink-0 place-items-center rounded-full transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2 active:bg-(--ii-selected)"
  >
    <FlaskConicalIcon class="size-4" aria-hidden="true" />
  </button>
{/snippet}

{#snippet overview(inSheet: boolean)}
  <div class="flex flex-col gap-5">
    <!-- The sheet on a phone needs a title, which keeps clear of its Close,
         and the identity over the cards; beside the canvas the identity
         heads the whole panel instead (see `panel`). -->
    {#if inSheet}
      <h2
        class="text-text-primary flex h-12 items-center pe-10 text-base font-semibold"
      >
        Identity
      </h2>
      <div class="mb-1">
        {@render identityItem()}
      </div>
    {/if}
    <!-- Three independent cards, each a large target that leads to its
         section: the title, where it stands, and what's inside. On desktop a
         card grows into its section in place: it rises to the top of the
         list, just under the identity, as it opens, and the other two fade
         where they stand (held there out of the flow, then gone), so the
         section has the panel to itself until Close shrinks it back into its
         slot. The cards stand apart, each one a thing of its own. -->
    <div bind:this={morphList} class="relative flex flex-col">
      {#each accountCards as card, index (card.kind)}
        {@render overviewItem(
          card.kind,
          grown === card.kind || index === 0 ? "" : "mt-4",
          card,
        )}
      {/each}
    </div>
  </div>
{/snippet}

{#snippet overviewItem(id: string, space: string, card: CardStatus)}
  {@const at = heldAt(id)}
  <div
    data-morph={id}
    inert={grown !== undefined && grown !== id}
    style:top={at !== undefined ? `${at.top}px` : undefined}
    style:left={at !== undefined ? `${at.left}px` : undefined}
    style:width={at !== undefined ? `${at.width}px` : undefined}
    class={[
      space,
      (grown === id || morphing === id) && "relative z-1",
      grown !== undefined &&
        grown !== id &&
        (at !== undefined ? "pointer-events-none absolute" : "hidden"),
    ]}
  >
    {@render statusCard(card)}
  </div>
{/snippet}

{#snippet identityItem()}
  <IdentityCard
    {identity}
    others={identities
      .map((entry) => entry.identity)
      .filter((one) => one.identityNumber !== identity.identityNumber)}
    bind:switching
    signedInWith={signedInWith.title}
    {paletteOf}
    colour={colourIndex}
    onColour={pickColour}
    onSwitch={switchIdentity}
    onAdd={() => unavailable("Adding an identity")}
    onStartPage={showStartPage}
    onSignOut={() => unavailable("Signing out")}
  >
    {#snippet alert()}
      {#if incident !== undefined}
        <div
          class="px-2 pb-2"
          out:slide={{ duration: reduced ? 0 : 260, easing: quintOut }}
        >
          <IncidentCard {incident} onAnswer={answerIncident} />
        </div>
      {/if}
    {/snippet}
  </IdentityCard>
{/snippet}

{#snippet statusCard(card: CardStatus)}
  <Elevated offset={1} data-morph-box class="overflow-hidden rounded-2xl">
    <StatusCard
      kind={card.kind}
      title={TITLES[card.kind]}
      sentence={card.sentence}
      tone={card.tone}
      loading={!known}
      resolved={resolvedKinds.includes(card.kind)}
      open={grown === card.kind}
      morphing={morphing === card.kind}
      onOpen={() => openCard(card.kind)}
      onClose={closeSection}
      glyphs={glyphsOf(card, current.scenario, browsers, signedInWith.title)}
    >
      {@render sectionOf(card.kind as Section, "card")}
    </StatusCard>
  </Elevated>
{/snippet}

{#snippet sectionOf(kind: Section, variant: "card" | "page")}
  <SectionView
    section={kind}
    {variant}
    scenario={current.scenario}
    card={cards.find((card) => card.kind === kind)}
    {signedInWith}
    {browsers}
    onClose={closeSection}
    onSignOut={signOut}
    onResolve={resolve}
    onRecovery={openRecovery}
    onUnavailable={unavailable}
  />
{/snippet}

{#snippet panelBody(inSheet: boolean)}
  <!-- Beside the canvas an app's options, and managing apps, are one card
       with Close, as an open section is (Thomas: the accordion, applied to
       them too); a phone's sheet keeps them as views with Back or Done. -->
  {@const variant = inSheet ? "page" : "card"}
  {#if managing}
    {#if selectedApps.length === 1}
      {@const app = selectedApps[0]}
      <AppView
        {app}
        {variant}
        history={current.keepHistory}
        backLabel="Done"
        onExit={stopManaging}
        onToggleNotifications={(allowed) => setNotifications([app], allowed)}
        onRemove={() => askRemove([app])}
      />
    {:else if selectedApps.length > 1}
      <BulkView
        apps={selectedApps}
        {variant}
        onExit={stopManaging}
        onSetNotifications={(allowed) =>
          setNotifications(selectedApps, allowed)}
        onRemove={() => askRemove(selectedApps)}
      />
    {:else if inSheet}
      <!-- Nothing picked yet: the panel says what can be done here. -->
      <div class="flex flex-col gap-4">
        <PanelHeader
          title="Manage apps"
          lead="Select apps to change their permissions or remove them."
          backLabel="Done"
          onBack={stopManaging}
        />
        <ManageHints />
      </div>
    {:else}
      <PanelCard title={TITLES.apps} closeLabel="Done" onClose={stopManaging}>
        {#snippet icon(large: boolean)}
          <span
            aria-hidden="true"
            class={[
              "text-fg-secondary grid shrink-0 place-items-center bg-(--ii-selected)",
              large ? "size-11 rounded-xl" : "size-9 rounded-[0.625rem]",
            ]}
          >
            <Grid2x2CheckIcon class={large ? "size-5.5" : "size-4.5"} />
          </span>
        {/snippet}
        {#snippet lead()}
          Select apps to change their permissions or remove them.
        {/snippet}
        <ManageHints variant="card" />
      </PanelCard>
    {/if}
  {:else if openedApp !== undefined}
    {@const app = openedApp}
    <AppView
      {app}
      {variant}
      history={current.keepHistory}
      onExit={closeAppDetails}
      onToggleNotifications={(allowed) => setNotifications([app], allowed)}
      onRemove={() => askRemove([app])}
      onOpen={() => {
        readAll(app);
        markUsed(app);
      }}
      onRead={(id) => read(app, id)}
      onReadAll={() => readAll(app)}
    />
  {:else if sectionPage !== undefined}
    {@render sectionOf(sectionPage, "page")}
  {:else}
    {@render overview(inSheet)}
  {/if}
{/snippet}

{#snippet panel(inSheet: boolean)}
  <!-- Beside the canvas the identity heads the panel, one compact card over
       every view (the overview, a section, an app's details, managing apps),
       so the head never changes and it's always clear whose these are; only
       the view under it does. Views stack in one cell, so the old one fades
       while the new one arrives. A phone's sheet doesn't repeat the name
       (Thomas): the page behind it already says whose it is, and only its
       overview shows the identity, over its cards. -->
  {#if !inSheet}
    <div class="mb-6">
      {@render identityItem()}
    </div>
  {/if}
  <div class="grid">
    {#key viewKey}
      <div
        class="col-start-1 row-start-1 min-w-0"
        in:viewIn={{ direction: navDirection * readingDirection(), reduced }}
        out:viewOut={{ reduced }}
      >
        {@render panelBody(inSheet)}
      </div>
    {/key}
  </div>
{/snippet}

<!-- On a phone the canvas is the whole screen. On desktop the apps are in a
     frame on the page, and the panel lies on the page beside it, a step
     lower, so the two read as two zones. The page is the canvas's own colour
     on a phone, where only an overscroll shows it.
     --ii-edge: a phone's margin, the same on both sides, which the logo,
     the identity, the greeting, the cards, the tabs, the apps and the pages'
     content all stand on; plates (hover, a lifted pill) reach past it. -->
<div
  class="concept text-text-primary min-h-dvh bg-(--ii-canvas) [--ii-edge:--spacing(4)] md:flex md:h-dvh md:bg-(--ii-page) md:p-2"
>
  <!-- The canvas: the apps, in a frame on the page. It doesn't scroll: the
       header stays put and each page scrolls on its own under it. -->
  <Elevated
    offset={2}
    as="main"
    class="@container/canvas flex h-dvh min-w-0 flex-1 flex-col overflow-hidden max-md:rounded-none max-md:border-0 max-md:shadow-none md:h-auto md:min-h-0 md:rounded-[1.25rem]"
  >
    <div class="h-[env(safe-area-inset-top)] shrink-0"></div>
    <h1 class="sr-only">Apps</h1>

    <!-- The header: on top, the logo and the controls; under them, whose
         canvas this is and its pages, lined up with the grid. Each control
         is a pill (see `pill`); between them the pages show through. From
         `md` the inset is 20px, where the account's pill ends. -->
    <header
      bind:this={headerElement}
      bind:clientHeight={headerHeight}
      class="pointer-events-none relative z-10 flex shrink-0 flex-col"
    >
      <!-- Over the grid, three columns: the logo, the tabs, the controls.
           The outer two share what's left, so the tabs stand on the canvas's
           middle, over the apps. A canvas too narrow for the three in a row
           puts the tabs on a row of their own under them, still centred. -->
      <div
        class={[
          "grid items-center gap-x-1 px-4 sm:px-6 md:gap-x-4 md:px-5 @max-lg/canvas:px-(--ii-edge)",
          HEADER_SPAN,
          phoneHead
            ? "grid-cols-[1fr_auto]"
            : "grid-cols-[1fr_auto] @[36rem]/canvas:grid-cols-[1fr_auto_1fr]",
        ]}
      >
        <!-- The logo is the way home: it goes back to the first page. Its hover
             is a layer over the pill's own colour (`ii-hover`), so the pill
             stays solid over the pages when it is lifted. -->
        <button
          type="button"
          dir="ltr"
          onclick={() => goToPage(0)}
          aria-label={`id.ai, go to ${PAGES[0].label}`}
          class={[
            pill,
            PILL_MOTION,
            "ii-hover focus-visible:outline-focus-ring my-3 flex h-10 items-center gap-3 justify-self-start px-4 focus-visible:outline-2 focus-visible:outline-offset-2",
            // On a phone the glyph stands on the edge; the pill reaches 10px
            // past it once lifted. Over a wide canvas the glyph stands on
            // the first logo's edge, and the pill reaches its 16px and its
            // border past it.
            "@max-lg/canvas:-ms-2.5 @max-lg/canvas:px-[9px] @[72rem]/canvas:-ms-[17px]",
          ]}
        >
          <Logo class="text-fg-primary h-4" />
          <span class="text-text-primary text-base font-semibold"> id.ai </span>
        </button>
        {#if !phoneHead}
          <div
            class="col-span-2 row-start-2 mb-3 justify-self-center @[36rem]/canvas:col-span-1 @[36rem]/canvas:col-start-2 @[36rem]/canvas:row-start-1 @[36rem]/canvas:mb-0"
          >
            {@render pageTabs(false)}
          </div>
        {/if}
        <!-- Desktop: search's glyph in the canvas's top corner, on every
             page and however far it's scrolled, so an app is found from
             wherever you are (⌘K too). Its pill ends where the account's
             would, 20px in; over a wide canvas, over the last logo's edge
             (HEADER_SPAN). -->
        <div
          class={[
            "col-start-2 row-start-1 flex min-w-0 items-center justify-end gap-2",
            !phoneHead && "@[36rem]/canvas:col-start-3",
          ]}
        >
          {#if isDesktop.current}
            <HeaderSearch
              onclick={() => search?.show()}
              lifted={headerLifted}
            />
          {/if}
          <div
            bind:this={controlsPill}
            class={[
              "flex shrink-0 items-center gap-1",
              layout === "panel" && "md:hidden",
              // A phone's controls share one pill; on desktop the account, or
              // Close, has its own.
              !isDesktop.current && [pill, PILL_MOTION, "h-10 px-[3px]"],
              // The avatar on the end edge, the pill 10px past it.
              phoneHead && "@max-lg/canvas:-me-2.5 @max-lg/canvas:pe-1.5",
            ]}
          >
            <!-- Phone only, beside the island or the dock: on desktop, and on a
               phone in the main layout, it keeps to the bottom-left corner. -->
            {#if !phoneHead}
              <div class="flex md:hidden">
                {@render conceptControls(true)}
              </div>
            {/if}
            {#if isDesktop.current && panelVisible}
              <!-- Desktop, with the panel open: Close stands where the account
                 was, on the canvas, which is what the panel pushed aside. The
                 glyph is an arrow to the edge: the panel goes back to the right
                 edge it came from. The identity itself heads the panel now, so
                 the canvas doesn't name it twice. -->
              <button
                bind:this={closeButton}
                type="button"
                onclick={() => (panelOpen = false)}
                title="Close"
                in:fade={{ duration: reduced ? 0 : 200 }}
                class={[
                  pill,
                  PILL_MOTION,
                  "ii-hover text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring grid size-10 place-items-center focus-visible:outline-2 focus-visible:outline-offset-2",
                ]}
              >
                <ArrowRightToLineIcon
                  class="size-4 rtl:-scale-x-100"
                  aria-hidden="true"
                />
                <span class="sr-only">Close identity</span>
              </button>
            {:else if layout === "top"}
              <!-- The account, name then avatar at the edge; it opens the panel
                 on desktop and the sheet on a phone. On desktop it has a pill of
                 its own; on a phone it shares the controls'. -->
              <button
                bind:this={accountButton}
                type="button"
                onclick={openAccount}
                aria-label={`${identityName}. Open identity${accountAttention !== undefined ? ", needs your attention" : ""}`}
                in:fade={{ duration: reduced ? 0 : 200 }}
                class={[
                  "ii-hover focus-visible:outline-focus-ring flex min-w-0 items-center gap-2.5 rounded-full ps-3 focus-visible:outline-2 focus-visible:outline-offset-2",
                  isDesktop.current
                    ? [pill, PILL_MOTION, "h-10 pe-[3px]"]
                    : "-me-1 py-1 pe-1",
                ]}
              >
                <span
                  class="text-text-primary max-w-28 truncate text-sm font-medium md:max-w-40"
                >
                  {identityName}
                </span>
                <!-- The dot cuts itself out of the pill the account stands on. -->
                <span
                  class="relative"
                  style:--ii-dot-backdrop={headerLifted
                    ? "var(--ii-floating)"
                    : "var(--ii-canvas)"}
                >
                  <IdentityAvatar
                    {identity}
                    palette={identityPaletteOverride}
                    size="sm"
                  />
                  {#if accountAttention !== undefined}
                    <AttentionDot tone={accountAttention} />
                  {/if}
                </span>
              </button>
            {/if}
            {#if phoneHead && headCollapsed}
              <!-- Phone, with the head scrolled away: its three sections stay
                 at the top as icons, each with its dot, a tap away. -->
              <div
                transition:fade={{ duration: reduced ? 0 : 160 }}
                class="flex items-center"
                style:--ii-dot-backdrop={headerLifted
                  ? "var(--ii-floating)"
                  : "var(--ii-canvas)"}
              >
                {#each accountCards as card (card.kind)}
                  {@const Icon = SECTION_ICONS[card.kind]}
                  {@const dot = known ? dotOf(card.tone) : undefined}
                  <button
                    type="button"
                    onclick={() => openFromHead(card.kind)}
                    aria-label={`${TITLES[card.kind]}: ${card.sentence}`}
                    class="text-fg-secondary focus-visible:outline-focus-ring grid size-8 place-items-center rounded-full transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:outline-2 active:bg-(--ii-selected)"
                  >
                    <span class="relative">
                      <Icon class="size-4" aria-hidden="true" />
                      {#if dot !== undefined}
                        <AttentionDot tone={dot} />
                      {/if}
                    </span>
                  </button>
                {/each}
              </div>
            {/if}
            {#if !isDesktop.current}
              <!-- Phone: search is the header's too, a glyph beside the
                 identity, and opens over the whole screen. In the main
                 layout it keeps its place next to the avatar while the
                 sections' icons come and go before it. -->
              <HeaderSearch
                compact
                onclick={() => search?.show()}
                class={!phoneHead ? "order-first" : undefined}
              />
            {/if}
            {#if phoneHead}
              <!-- Phone: the identity at the end of the top row, as an account
                 sits in an app's corner. The greeting already names it, so
                 the avatar is the way to the others and to its own settings:
                 a tap lists every identity on the device, then the colour,
                 the start page and signing out. -->
              <IdentityMenu
                trigger={{
                  content: headAvatar,
                  label: `${identityName}: identities and settings`,
                  class:
                    "focus-visible:outline-focus-ring grid shrink-0 place-items-center rounded-full p-[3px] transition-[background-color,scale] duration-150 ease-out hover:bg-(--ii-hover) focus-visible:outline-2 active:scale-[0.96] aria-expanded:bg-(--ii-selected) motion-reduce:active:scale-100",
                }}
                head={headIdentities}
                onColour={(button) => (headColourAnchor = button)}
                onStartPage={showStartPage}
                onSignOut={() => unavailable("Signing out")}
              />
              {#if headColourAnchor !== undefined}
                <ColourStrip
                  anchor={headColourAnchor}
                  align="end"
                  initial={identityInitial(identity.name)}
                  current={colourIndex}
                  onPick={pickColour}
                  onClose={() => (headColourAnchor = undefined)}
                />
              {/if}
            {/if}
          </div>
        </div>
      </div>
    </header>

    <!-- Pulled up by the header's height, so the pages run on under it. -->
    <div
      class="relative -mt-(--header-h) flex min-h-0 flex-1 flex-col"
      style:--header-h={`${headerHeight}px`}
    >
      <!-- The pages side by side: the tabs move between them, and on touch or a
         trackpad a swipe does too, landing on a page. -->
      <div
        bind:this={pager}
        onscrollcapture={onPageScroll}
        onscrollend={onPagerScrollEnd}
        class="flex min-h-0 flex-1 snap-x snap-mandatory [scrollbar-width:none] overflow-x-auto overscroll-x-contain [&::-webkit-scrollbar]:hidden"
      >
        <div
          bind:this={pageRefs[0]}
          id="page-mine"
          role="tabpanel"
          aria-labelledby="tab-mine"
          class={[PAGE, settled[0] && SETTLED]}
        >
          {#if phoneHead}
            {@render pageHead(0)}
          {:else}
            <div
              bind:this={mineTop}
              aria-hidden="true"
              class={MINE_TOP}
              style:flex={mineTopHeld === undefined
                ? undefined
                : `0 0 ${mineTopHeld}px`}
            ></div>
          {/if}
          <!-- As the page loads its blocks arrive, the title first, 60ms
               apart (`arriveIn`); on a phone after the head over them. -->
          <div
            use:arriveIn={{ visit, reduced, delay: phoneHead ? 120 : 0 }}
            class="mx-auto flex w-fit max-w-full flex-col @max-lg/canvas:w-full"
          >
            <!-- The page says whose apps these are before anything else, the
                 way a person would: the name, with a dot in the identity's
                 colour before it. The name is said here only; the panel's
                 card is for switching. Across from it, Edit (Done while apps
                 are arranged), as over a list on iOS. The row is as wide as
                 the grid (w-0 min-w-full), the name starting on the first
                 logo's edge and Edit's pill reaching 4px past the last one,
                 as the setting card under it does. -->
            {#if !phoneHead}
              <div
                bind:this={pageHeads[0]}
                class={[
                  "flex w-0 min-w-full items-center justify-between gap-3 @lg:pe-[calc((var(--ii-tile)-var(--ii-logo))/2)]",
                  INSET,
                  HEAD_ROW,
                ]}
              >
                {@render greetingLine(false)}
                <!-- Only once the apps are there to arrange. -->
                {#if appsLoad.state === "ready" && current.apps.length > 0}
                  <button
                    type="button"
                    in:fade={{ duration: reduced ? 150 : 200 }}
                    onclick={managing ? stopManaging : startManaging}
                    class="ii-hover text-text-secondary hover:text-text-primary focus-visible:outline-focus-ring -me-1 h-10 shrink-0 rounded-full border border-transparent px-3.5 text-sm font-medium focus-visible:outline-2 @max-lg/canvas:-me-2.5 @max-lg/canvas:px-[9px]"
                  >
                    {@render editWords("@max-lg/canvas:justify-items-end")}
                  </button>
                {/if}
              </div>
            {/if}
            <!-- Phone: an incident put away unanswered heads the apps. -->
            {#if !isDesktop.current && incident !== undefined && !managing}
              <div
                transition:slide={{
                  duration: reduced ? 0 : 260,
                  easing: quintOut,
                }}
                class="mx-2.5 pb-8 @lg:-mx-2 @lg:w-0 @lg:min-w-[calc(100%+1rem)]"
              >
                <IncidentBanner
                  {incident}
                  class="w-full"
                  onOpen={() => (incidentDismissed = false)}
                />
              </div>
            {/if}
            <!-- Until the apps land, their tiles' skeleton in the grid's place
                 (Swap); then the grid, its tiles one after another. -->
            <Swap status={appsLoad.state} items=".ii-tiles > li, .ii-tiles ~ *">
              {#snippet skeleton()}
                {@render tilesSkeleton()}
              {/snippet}
              {#snippet failed()}
                {@render appsFailed()}
              {/snippet}
              {#if current.apps.length > 0}
                <!-- Named by its tab, right above it. -->
                <section aria-labelledby="tab-mine" class="flex flex-col">
                  <!-- While apps are arranged, a privacy setting heads the grid.
                     Its room opens as the grid glides down and the card fades
                     in within it; on Done the card fades first and the room
                     closes. It stays mounted, inert while hidden, so a quick
                     Edit and Done turn around midway instead of starting
                     over. It is as wide as the grid (w-0 min-w-full): a long
                     line can't widen the column and shift the grid sideways.
                     Over the grid the card spans what the header row's
                     controls do: from the tabs' track, 4px past the first
                     logo, to Done's pill, 4px past the last one. Under the
                     header it keeps the same room above as below. -->
                  <div
                    inert={!managing}
                    class={[
                      "grid w-0 min-w-full transition-[grid-template-rows] duration-250 ease-(--ii-ease-out) motion-reduce:transition-none",
                      managing ? "grid-rows-[1fr]" : "grid-rows-[0fr]",
                    ]}
                  >
                    <div class="min-h-0 overflow-hidden">
                      <!-- A card a step above the canvas, as anything laid
                           on it is: in dark lighter than the canvas, not a
                           hole in it. -->
                      <label
                        style:background-color={surfaceColor(CANVAS + 1)}
                        class={[
                          "border-border-tertiary mx-2.5 mb-5 flex origin-top items-center gap-4 rounded-2xl border px-4 py-3 transition-[opacity,scale] motion-reduce:scale-100 md:mt-2 @lg:mx-[calc((var(--ii-tile)-var(--ii-logo))/2-var(--spacing))]",
                          managing
                            ? "scale-100 opacity-100 delay-50 duration-200 ease-(--ii-ease-out)"
                            : "scale-[0.98] opacity-0 duration-120 ease-out",
                        ]}
                      >
                        <!-- A privacy setting, not a list setting: it decides
                           whether there is any record of where this identity
                           signs in. -->
                        <span class="flex min-w-0 flex-1 flex-col gap-0.5">
                          <span class="text-text-primary text-sm font-medium">
                            Remember apps I sign in to
                          </span>
                          <span
                            class="text-text-tertiary text-[13px] leading-[18px] text-pretty"
                          >
                            {current.keepHistory
                              ? "An app shows up here once you sign in to it."
                              : "No record of where you sign in. Only apps you add yourself show up here, and signing in still works everywhere."}
                          </span>
                        </span>
                        <Toggle
                          checked={current.keepHistory}
                          onchange={(event) =>
                            (current.keepHistory = event.currentTarget.checked)}
                        />
                      </label>
                    </div>
                  </div>
                  <ReorderGrid
                    items={gridItems}
                    key={(item) => item.origin}
                    editing={managing}
                    onMove={moveApp}
                    pinned={(item) => !("name" in item)}
                    class={`${GRID} ii-tiles`}
                  >
                    {#snippet tile(item, index)}
                      {#if "name" in item}
                        {@render appTile(item, index)}
                      {:else}
                        {@render addTile()}
                      {/if}
                    {/snippet}
                  </ReorderGrid>
                  {#if phoneHead}
                    <!-- Phone: the tabs are down in the tab bar, so Edit stands
                       under the apps, as iOS puts Edit under the widgets. -->
                    <button
                      type="button"
                      onclick={managing ? stopManaging : startManaging}
                      class="btn btn-secondary btn-sm mt-6 self-center rounded-full px-5"
                    >
                      {@render editWords("")}
                    </button>
                  {/if}
                </section>
              {:else}
                {@render firstApps()}
              {/if}
            </Swap>
          </div>
        </div>

        <div
          bind:this={pageRefs[1]}
          id="page-explore"
          role="tabpanel"
          aria-labelledby="tab-explore"
          class={[PAGE, settled[1] && SETTLED]}
        >
          {#if phoneHead}
            {@render pageHead(1)}
          {:else}
            <div aria-hidden="true" class={PAGE_TOP}></div>
          {/if}
          <!-- Explore reads like the App Store: a banner, a shelf, another
               banner, more shelves. The featured shelf keeps to apps this
               identity doesn't use yet, and steps aside when there are none. -->
          <div class={["mx-auto", LOGO_SPAN]}>
            {#if !phoneHead}
              {@render pageIntroRow(1)}
            {/if}
            <!-- Its feed leaves out the apps already added, so it waits for
                 them; and so its pictures don't take the network from My
                 apps' logos as the page loads. -->
            {#if appsLoad.state !== "loading"}
              <ExplorePage {featured} {isAdded} onAdd={(app) => addApp(app)} />
            {/if}
          </div>
        </div>

        <!-- Connect: AI and command-line access to this identity's apps,
             with their state, as the identity's own page. -->
        <div
          bind:this={pageRefs[2]}
          id="page-connect"
          role="tabpanel"
          aria-labelledby="tab-connect"
          class={[PAGE, settled[2] && SETTLED]}
        >
          {#if phoneHead}
            {@render pageHead(2)}
          {:else}
            <div aria-hidden="true" class={PAGE_TOP}></div>
          {/if}
          <div class={["mx-auto", LOGO_SPAN]}>
            {#if !phoneHead}
              {@render pageIntroRow(2)}
            {/if}
            <!-- It lists the apps and the account's integrations, so it
                 waits for both. -->
            {#if appsLoad.state !== "loading" && known}
              <ConnectPage
                integrations={current.scenario.integrations}
                apps={current.apps}
                onResolve={resolve}
                onUnavailable={unavailable}
              />
            {/if}
          </div>
        </div>
      </div>
      <!-- Content comes up from under what floats at the bottom (a phone's
           tab bar, an island) through a blur that deepens into the canvas.
           A page keeps clear of it at rest (PAGE's pb-32). Beside the panel
           nothing floats there, so the frame's edge ends the page plainly. -->
      {#if !isDesktop.current || layout !== "panel"}
        <ProgressiveBlur side="bottom" fade class="h-24" />
      {/if}
      <!-- What scrolls up leaves under the header through a blur too, rather
           than at the canvas's edge: a phone's head cards, and over the grid
           whatever passes under the logo, the tabs and search. -->
      <ProgressiveBlur side="top" fade class="h-(--header-h)" />
    </div>
  </Elevated>

  <!-- Desktop: the panel lies on the page beside the canvas. With the island it
       opens and closes; the canvas gives it the room. -->
  <div
    class="h-full shrink-0 overflow-hidden transition-[width] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none max-md:hidden"
    style:width={panelVisible ? "25rem" : "0rem"}
  >
    <aside
      bind:this={aside}
      aria-label="Identity"
      inert={!panelVisible}
      class={[
        // 24px of air on both sides: from the canvas frame (8px gap + 16px)
        // and from the window edge (8px page padding + 16px). The first card
        // starts on the top of the canvas header's pills (8px page padding,
        // 1px, then 12px).
        "flex h-full w-[25rem] flex-col overflow-y-auto overscroll-contain ps-6 pe-4 pt-[13px] pb-4",
        "transition-[opacity,translate] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
        !panelVisible && "translate-x-4 opacity-0 rtl:-translate-x-4",
      ]}
    >
      {#if isDesktop.current}
        <!-- The panel lies on the page, but its cards stand level with the
             canvas (3): a step lower, beside it, they read as sunk into the
             page. So all it holds counts as standing on 2. -->
        <!-- As the page loads, the identity and then the cards arrive, a
             step after the canvas's title (`arriveIn`). -->
        <div class="shrink-0" use:arriveIn={{ visit, reduced, delay: 60 }}>
          <SurfaceLevel level={2}>
            {@render panel(false)}
          </SurfaceLevel>
        </div>
        <!-- Concept only, to try both themes: it keeps to the panel's foot, at
             the bottom of the window while the panel is short, and after the
             last card once it is long. Two glyphs on a track, the one in use
             raised, as a footer's theme switch is on Vercel's pages. The
             switch alone, without a caption: its glyphs say what it does. -->
        <div class="mt-auto flex shrink-0 justify-end pt-6">
          <div
            role="radiogroup"
            aria-label="Theme"
            class="flex rounded-full bg-(--ii-hover) p-0.5"
          >
            {#each THEMES as option (option.value)}
              {@const Icon = option.icon}
              {@const checked = theme.current === option.value}
              <button
                type="button"
                role="radio"
                aria-checked={checked}
                title={option.label}
                onclick={() => theme.set(option.value)}
                class={[
                  "focus-visible:outline-focus-ring grid size-7 place-items-center rounded-full transition-[color,background-color,box-shadow] duration-150 ease-out focus-visible:outline-2 motion-reduce:transition-none",
                  checked
                    ? "text-fg-primary bg-(--ii-top) shadow-(--ii-shadow-2)"
                    : "text-fg-quaternary hover:text-fg-tertiary",
                ]}
              >
                <Icon class="size-3.5" aria-hidden="true" />
                <span class="sr-only">{option.label}</span>
              </button>
            {/each}
          </div>
        </div>
      {/if}
    </aside>
  </div>

  <!-- Phone: an unanswered incident meets the user over the apps on every
       visit, until they answer it. Put away, it waits at the top of the apps. -->
  {#if !isDesktop.current && incident !== undefined && !incidentDismissed && !panelOpen}
    <Sheet onClose={() => (incidentDismissed = true)}>
      <IncidentCard {incident} variant="sheet" onAnswer={answerIncident} />
    </Sheet>
  {/if}

  <!-- Phone (Thomas): the panel is a sheet from the bottom, with a grabber
       to swipe it away. A section brings its own Close, across from its
       title. -->
  {#if panelOpen && !isDesktop.current}
    <Sheet
      onClose={() => (panelOpen = false)}
      showCloseButton={sectionPage === undefined}
    >
      {@render panel(true)}
    </Sheet>
  {/if}

  {#if phoneHead && !managing}
    <!-- Phone: the pages are the tab bar, fixed at the bottom over the blur
         the pages fade into. While apps are arranged the selection bar takes
         its place, as a phone's toolbar does in a selection. -->
    <div
      in:shellIn={{ reduced }}
      out:shellOut={{ reduced }}
      class="pointer-events-none fixed inset-x-0 bottom-[calc(1rem+env(safe-area-inset-bottom))] z-20 flex justify-center px-3"
    >
      {@render pageTabs(true)}
    </div>
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
      anyNotifying={selectedApps.some((app) => app.notificationsAllowed)}
      onOpen={() => (panelOpen = true)}
      onAction={() => lead !== undefined && runLead(lead)}
      onDetails={() => (panelOpen = true)}
      onSetNotifications={(allowed) =>
        setNotificationsAndSay(selectedApps, allowed)}
      onRemove={() => askRemove(selectedApps)}
      onDone={stopManaging}
    />
  {/if}

  {#if layout === "top" && !panelVisible && !managing && dockItems.length > 0}
    <ActionDock items={dockItems} onAction={actFromDock} />
  {/if}

  <!-- The page's one search, over everything: the header's glyph opens it,
       ⌘K on desktop, and Add app after the last tile. A new one for a phone
       or a desktop, as the header's glyph is. -->
  {#key isDesktop.current}
    <SearchOverlay
      bind:this={search}
      compact={!isDesktop.current}
      apps={current.apps}
      {isAdded}
      onAdd={(app) => addApp(app)}
      onAddLink={addLink}
      onOpen={openedFromSearch}
      suggested={featured}
    />
  {/key}

  {#if recoveryOpen}
    <RecoveryFlow
      recovery={current.scenario.recovery}
      {identityName}
      focus={recoveryFocus}
      onChange={resolve}
      onClose={() => (recoveryOpen = false)}
      sheet={!isDesktop.current}
    />
  {/if}

  {#if removing !== undefined}
    {@const apps = removing}
    {@const what = apps.length === 1 ? apps[0].name : `${apps.length} apps`}
    {#snippet confirmRemove()}
      <div class="flex flex-col gap-5">
        <h2
          id="concept-remove-title"
          class="text-text-primary text-lg font-semibold"
        >
          Remove {what}?
        </h2>
        <ul class="text-text-secondary flex flex-col gap-3 text-sm">
          <li class="flex items-center gap-3">
            <LogOutIcon
              class="text-fg-tertiary size-4 shrink-0 rtl:-scale-x-100"
              aria-hidden="true"
            />
            Signs you out of {apps.length === 1 ? apps[0].name : "them"}
          </li>
          <li class="flex items-center gap-3">
            <BellOffIcon
              class="text-fg-tertiary size-4 shrink-0"
              aria-hidden="true"
            />
            Turns off {apps.length === 1 ? "its" : "their"} notifications
          </li>
          <li class="flex items-center gap-3">
            <CompassIcon
              class="text-fg-tertiary size-4 shrink-0"
              aria-hidden="true"
            />
            {apps.length === 1 ? "It stays" : "They stay"} in Explore if you want
            {apps.length === 1 ? " it" : " them"} back
          </li>
        </ul>
        <div class="flex gap-2">
          <button
            type="button"
            onclick={() => (removing = undefined)}
            class="btn btn-secondary flex-1"
          >
            Cancel
          </button>
          <button
            type="button"
            onclick={() => {
              removing = undefined;
              remove(apps);
            }}
            class="btn btn-primary btn-danger flex-1"
          >
            Remove
          </button>
        </div>
      </div>
    {/snippet}
    <!-- Phone: a sheet, as every other one there, rather than the product's
         own; desktop keeps the product's dialog. -->
    {#if isDesktop.current}
      <Dialog onClose={() => (removing = undefined)}>
        {@render confirmRemove()}
      </Dialog>
    {:else}
      <Sheet
        onClose={() => (removing = undefined)}
        aria-labelledby="concept-remove-title"
      >
        {@render confirmRemove()}
      </Sheet>
    {/if}
  {/if}

  <!-- Concept only: walk the panel through its states, and switch layouts.
       On desktop it stays in the bottom-left corner in every layout. A phone
       in the main layout has it in the same corner, its glyph under the
       logo's, since the header's end is the identity's; it steps aside for
       the selection bar while apps are arranged. In the other two layouts a
       phone has it in the header, leaving the bottom to the island and the
       dock. -->
  {#if isDesktop.current || (phoneHead && islandView === undefined)}
    <!-- On a phone it's level with the tab bar's middle (48px tall, the glyph
         32px). -->
    <div
      transition:fade={{ duration: reduced ? 0 : 150 }}
      class="fixed start-[calc(var(--ii-edge)-0.5rem)] bottom-[calc(1.5rem+env(safe-area-inset-bottom))] z-10 min-[32rem]:start-[25px] sm:start-[33px] md:start-4 md:bottom-4"
    >
      {@render conceptControls(false)}
    </div>
  {/if}

  {#if board}
    <StateBoard
      titles={TITLES}
      icons={SECTION_ICONS}
      {signedInWith}
      onClose={() => (board = false)}
    />
  {/if}

  {#if conceptMenu !== undefined}
    {@const close = () => (conceptMenu = undefined)}
    <Popup
      anchor={conceptMenu.anchor}
      onClose={close}
      direction={conceptMenu.compact ? "down" : "up"}
      align={conceptMenu.compact ? "end" : "start"}
      distance="0.5rem"
      responsive={false}
      class="!w-max"
    >
      <!-- On a short phone it scrolls within the room the popover leaves it. -->
      <div
        class="flex max-h-(--max-panel-height) flex-col overflow-y-auto overscroll-contain p-1.5 *:shrink-0"
        role="menu"
        aria-label="Concept settings"
      >
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
        <hr class="border-border-secondary -mx-1.5 my-1.5" />
        <div
          aria-hidden="true"
          class="text-text-tertiary px-3 pt-1.5 pb-1 text-xs"
        >
          Visit
        </div>
        <button
          type="button"
          role="menuitem"
          onclick={() => {
            close();
            arrive();
          }}
          class="btn btn-tertiary justify-start !px-3 text-start"
        >
          New visit
        </button>
        <button
          type="button"
          role="menuitem"
          onclick={() => {
            close();
            arriveFirst();
          }}
          class="btn btn-tertiary justify-start !px-3 text-start"
        >
          First visit
        </button>
        <hr class="border-border-secondary -mx-1.5 my-1.5" />
        <div
          aria-hidden="true"
          class="text-text-tertiary px-3 pt-1.5 pb-1 text-xs"
        >
          Data arrives
        </div>
        {#each LOAD_SPEEDS as option (option.value)}
          <button
            type="button"
            role="menuitemradio"
            aria-checked={option.value === loadSpeed}
            onclick={() => {
              close();
              setLoadSpeed(option.value);
            }}
            class="btn btn-tertiary justify-start gap-2.5 !px-3 text-start"
          >
            <span class="flex-1">{option.label}</span>
            <CheckIcon
              class={`text-fg-tertiary size-4 ${option.value === loadSpeed ? "" : "invisible"}`}
              aria-hidden="true"
            />
          </button>
        {/each}
        <hr class="border-border-secondary -mx-1.5 my-1.5" />
        <button
          type="button"
          role="menuitem"
          onclick={() => {
            close();
            board = true;
          }}
          class="btn btn-tertiary justify-start !px-3 text-start"
        >
          All card states
        </button>
      </div>
    </Popup>
  {/if}
</div>
