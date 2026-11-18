<script lang="ts">
  import "./surfaces.css";
  import {
    ArrowRightToLineIcon,
    AtSignIcon,
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
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { brandIconOf } from "../manage/(authenticated)/devices/browsers";
  import {
    IDENTITY_PALETTES,
    identityInitial,
    identityPalette,
    identityPaletteIndex,
    type IdentityPalette,
  } from "$lib/utils/identityColors";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import ActionDock, { type DockItem } from "./components/ActionDock.svelte";
  import AppTile from "./components/AppTile.svelte";
  import AttentionDot from "./components/AttentionDot.svelte";
  import AppView from "./components/AppView.svelte";
  import BulkView from "./components/BulkView.svelte";
  import ConnectPage from "./components/ConnectPage.svelte";
  import Elevated from "./components/Elevated.svelte";
  import SurfaceLevel from "./components/SurfaceLevel.svelte";
  import Popup from "./components/Popup.svelte";
  import AppRow from "./components/AppRow.svelte";
  import AppSearch from "./components/AppSearch.svelte";
  import ExplorePage from "./components/ExplorePage.svelte";
  import ColourStrip from "./components/ColourStrip.svelte";
  import IdentityCard from "./components/IdentityCard.svelte";
  import IdentityList from "./components/IdentityList.svelte";
  import IdentityMenu from "./components/IdentityMenu.svelte";
  import IncidentBanner from "./components/IncidentBanner.svelte";
  import IncidentCard from "./components/IncidentCard.svelte";
  import Island, { type IslandView } from "./components/Island.svelte";
  import PanelHeader from "./components/PanelHeader.svelte";
  import ProgressiveBlur from "./components/ProgressiveBlur.svelte";
  import RecoveryFlow, {
    type RecoveryMethod,
  } from "./components/RecoveryFlow.svelte";
  import ReorderGrid from "./components/ReorderGrid.svelte";
  import SectionView, { type Section } from "./components/SectionView.svelte";
  import StatusCard from "./components/StatusCard.svelte";
  import VerifyPhraseDialog from "./components/VerifyPhraseDialog.svelte";
  import {
    browsersFor,
    featuredApps,
    identities,
    scenarioPresets,
    type ConceptApp,
    type FeaturedApp,
  } from "./mockData";
  import { shellIn, shellOut, viewIn, viewOut } from "./motion";
  import { ConceptTheme } from "./theme.svelte";
  import {
    deriveStatus,
    emailStatus,
    incidentOf,
    isOn,
    phraseStatus,
    type MethodStatus,
    isLoud,
    leadActionOf,
    leadOf,
    needsAttention,
    type CardKind,
    type CardStatus,
    type IncidentAnswer,
    type Scenario,
    type Tone,
  } from "./status";

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
      signedInWith: signedInWith.title,
    }),
  );
  const lead = $derived(leadOf(cards));
  /** A risk, told as one story the user answers; it speaks for its cards. */
  const incident = $derived(
    incidentOf({
      scenario: current.scenario,
      signedOut: new Set(current.signedOut),
    }),
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
  /** A way back in, as the recovery card's preview says it. */
  const RECOVERY_SAID: Record<MethodStatus, string> = {
    off: "not set up",
    unchecked: "not verified",
    pending: "waiting",
    on: "on",
    inUse: "in use",
    new: "reset",
  };
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
      : tone === "action" || tone === "queued" || tone === "pending"
        ? "action"
        : tone === "recent" || tone === "now"
          ? "news"
          : undefined;
  /** What a phone's head card says: what happened when something did,
   *  otherwise where it stands in a few words. */
  const briefOf = (card: CardStatus): string => {
    if (card.tone !== "calm" && card.tone !== "tip") return card.sentence;
    if (card.kind === "access") {
      return current.scenario.access === "one"
        ? "One way to sign in"
        : "3 ways to sign in";
    }
    if (card.kind === "devices") {
      return browsers.length === 1
        ? "1 browser"
        : `${browsers.length} browsers`;
    }
    return card.sentence;
  };
  /** What the calm island mentions: something new or waiting, never a tip. */
  const note = $derived(
    cards.find(({ tone }) =>
      (["queued", "pending", "now", "recent"] as const).some((t) => t === tone),
    ),
  );

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
  const PAGE =
    "flex w-full shrink-0 snap-start snap-always flex-col overflow-y-auto overscroll-y-contain px-2 @max-lg/canvas:px-[calc(var(--ii-edge)-0.625rem)] pt-[calc(var(--header-h)+0.75rem)] pb-32 scroll-pt-[calc(var(--header-h)+0.75rem)] [scrollbar-color:transparent_transparent] [scrollbar-gutter:stable_both-edges] [scrollbar-width:thin] sm:px-6";
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
    "grid grid-cols-[repeat(4,minmax(0,5.25rem))] justify-between gap-x-1 gap-y-1 @lg:grid-cols-[repeat(4,7rem)] @lg:gap-x-2 @2xl:grid-cols-[repeat(5,7rem)] @4xl:grid-cols-[repeat(7,7rem)]";
  // A heading starts at the edge of the first logo, like a shelf title on
  // iOS. The 4rem logo sits centered in its column, so the inset is half of
  // what the column leaves: (7rem − 4rem) / 2. On a phone the head is as
  // wide as the logos reach, so it needs none.
  const INSET = "@lg:ps-6";
  /** The grid's own width at each size, so the greeting and the tabs above
   *  the pages line up with the apps: columns and gaps of GRID. On a phone,
   *  from edge to edge, where the first and the last logo stand. */
  const GRID_WIDTH =
    "w-[calc(100%-2*var(--ii-edge))] @lg:w-[29.5rem] @2xl:w-[37rem] @4xl:w-[52rem] max-w-full";
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
  /** Where the add-an-app search hangs from, while it's open. */
  let addAnchor = $state<HTMLElement>();
  /** A phone's header controls. The menu hangs from this pill's end, at the
   *  screen's edge: from the flask, mid-pill, it would run off the start. */
  let controlsPill = $state<HTMLElement>();

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
  /** Whether the overview lists the identities to switch to. */
  let switching = $state(false);
  /** The desktop panel, which scrolls. */
  let aside = $state<HTMLElement>();

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
      : openedApp !== undefined
        ? `details:${openedApp.origin}`
        : (section ?? "overview"),
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

  const openCard = (kind: CardKind) => {
    if (kind === "recovery") {
      openRecovery();
      return;
    }
    navDirection = 1;
    switching = false;
    if (kind === "apps") {
      startManaging();
    } else {
      section = kind;
    }
  };

  const closeSection = () => {
    navDirection = -1;
    section = undefined;
    // A phone opened the section from its header, so back is back there.
    if (phoneHead) {
      panelOpen = false;
    }
  };

  /** A section from the phone's header: in the sheet, over the apps. */
  const openFromHead = (kind: CardKind) => {
    openCard(kind);
    if (kind !== "recovery") {
      panelOpen = true;
    }
  };

  const startManaging = () => {
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
            card.tone !== "recent" ||
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
    cards.some(({ tone }) => tone === "risk")
      ? "risk"
      : needsAttention(cards)
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

  /** A visit starts: as the page loads, and from the concept menu. */
  const arrive = () => {
    incidentDismissed = false;
    section = undefined;
    appOpen = undefined;
    switching = false;
    managing = false;
    selected = [];
    panelOpen = opensOnArrival();
  };
  arrive();

  // The open panel has shown what it holds now.
  $effect(() => {
    if (!panelVisible) {
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
  let activePage = $state(0);
  let pager = $state<HTMLElement>();
  const pageRefs = $state<HTMLElement[]>([]);
  const tabRefs = $state<HTMLElement[]>([]);
  /** Each tab's width, for the thumb under the selected one. The tabs sit
   *  edge to edge after the pill's 4px of padding, so a tab starts where the
   *  ones before it end. */
  const tabWidths = $state(PAGES.map(() => 0));
  const thumb = $derived(
    tabWidths.every((width) => width > 0)
      ? {
          start: 4 + tabWidths.slice(0, activePage).reduce((a, b) => a + b, 0),
          width: tabWidths[activePage],
        }
      : undefined,
  );

  // Over the pages, the canvas says whose it is (the greeting), then the
  // pages themselves (the tabs), lined up with the grid. As a page scrolls,
  // the greeting goes up with it and out of sight, and the tabs follow until
  // they stop under the logo: the pages stay a tap away however far down.
  /** Each page's scroll, which moves the head while that page is shown. */
  const pageScroll = $state(PAGES.map(() => 0));
  /** The greeting's height in the header: how far the head goes up before
   *  the tabs stop. */
  let greetingHeight = $state(0);
  // On a phone the head (the greeting and the cards) is the top of each page
  // instead, and the tabs are the tab bar at the bottom: a touch anywhere on
  // the head scrolls the page, as on any phone screen, and the cards go up
  // under the logo whole. Each page's copy, measured.
  const pageHeadHeights = $state(PAGES.map(() => 0));
  /** How far the head goes before it's out of sight. */
  const headHeight = $derived(
    phoneHead ? (pageHeadHeights[activePage] ?? 0) : greetingHeight,
  );
  const headLift = $derived(Math.min(pageScroll[activePage] ?? 0, headHeight));
  /** The head has gone up out of sight. */
  const headCollapsed = $derived(headHeight > 0 && headLift >= headHeight - 2);
  /** A page switch brings that page's head along, so it moves, not jumps. */
  let switchingPage = $state(false);
  $effect(() => {
    void activePage;
    switchingPage = true;
    const timer = setTimeout(() => (switchingPage = false), 320);
    return () => clearTimeout(timer);
  });
  // The header's controls are pills in the canvas's own colour, so at rest
  // they don't show. Once a page's content runs under the tabs they lift off
  // the canvas, as the dock's pills sit over it, and keep the header readable
  // over whatever runs under it. The content reaches them once the greeting
  // in the header is gone (a phone's is in the page) and the 0.75rem PAGE
  // keeps under them is passed.
  const headerLifted = $derived(
    (pageScroll[activePage] ?? 0) > (phoneHead ? 0 : greetingHeight) + 12,
  );
  /** The pages scroll on their own; the pager hears them while capturing. */
  const onPageScroll = (event: Event) => {
    const index = pageRefs.indexOf(event.target as HTMLElement);
    if (index !== -1) {
      pageScroll[index] = pageRefs[index].scrollTop;
    }
  };
  const pill = $derived([
    "pointer-events-auto rounded-full border border-transparent",
    headerLifted ? "ii-elevation-6 bg-(--ii-surface-6)" : "bg-(--ii-surface-3)",
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

  const goToPage = (index: number) => {
    if (index !== pageInView()) {
      headingTo = index;
    }
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

  /** The product's phrase check, while it's open, and what passing it resolves. */
  let verifying = $state<{ patch: Partial<Scenario>; message: string }>();
  const verifyPhrase = (patch: Partial<Scenario>, message: string) => {
    verifying = { patch, message };
  };
  const passVerify = () => {
    if (verifying === undefined) return;
    const { patch, message } = verifying;
    verifying = undefined;
    resolve(patch, message);
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
      const state = current.scenario.recovery;
      openRecovery(
        state === "unverified"
          ? "phrase"
          : state === "emailPending"
            ? "email"
            : undefined,
      );
      return;
    }
    const action = leadActionOf(card, current.scenario);
    if (action.resolve !== undefined) {
      if (action.flow === "verifyPhrase") {
        verifyPhrase(action.resolve.patch, action.resolve.message);
      } else {
        resolve(action.resolve.patch, action.resolve.message);
      }
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
      {@const Icon = SECTION_ICONS[card.kind]}
      <Elevated offset={1} class="overflow-hidden rounded-2xl">
        <button
          type="button"
          onclick={() => openFromHead(card.kind)}
          aria-label={`${TITLES[card.kind]}: ${card.sentence}`}
          class="ii-hover focus-visible:ring-focus-ring flex h-full w-full flex-col items-start gap-2 p-2.5 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset"
        >
          <span
            aria-hidden="true"
            class={[
              "grid size-7 shrink-0 place-items-center rounded-lg",
              {
                risk: "text-fg-error-primary bg-(--ii-error-tint)",
                action: "text-fg-warning-primary bg-(--ii-warning-tint)",
              }[card.tone as "risk" | "action"] ??
                "text-fg-secondary bg-(--ii-selected)",
            ]}
          >
            <Icon class="size-4" />
          </span>
          <span class="flex flex-col gap-0.5">
            <span class="text-text-primary text-[13px] leading-4 font-semibold">
              {card.kind === "access" ? "Access" : TITLES[card.kind]}
            </span>
            <span
              class={[
                "line-clamp-3 text-xs leading-4 text-pretty",
                card.tone === "risk"
                  ? "text-text-error-primary"
                  : card.tone === "calm" || card.tone === "tip"
                    ? "text-text-tertiary"
                    : "text-text-primary",
              ]}
            >
              {briefOf(card)}
            </span>
          </span>
        </button>
      </Elevated>
    {/each}
  </div>
{/snippet}

{#snippet greetingLine(inPage: boolean)}
  <!-- On a phone, at the top of the page, it's a step smaller, so a long name
       keeps to the line, and closer to the logo than to the cards under it. -->
  <p
    class={[
      "text-text-primary flex min-w-0 items-center gap-x-2 leading-7 font-semibold tracking-tight",
      INSET,
      inPage
        ? "pb-6 text-lg"
        : "pt-3 pb-5 text-xl md:pt-[clamp(1rem,8vh,5rem)] md:pb-6",
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

{#snippet pageHead(index: number)}
  <!-- Phone: the greeting and the cards head the page itself, so they scroll
       with it from wherever a touch starts. As wide as the grid (the page
       keeps 10px short of the edge, so they step in by those). Only the shown
       page's copy is live; the others are what a swipe brings in. -->
  <div
    bind:clientHeight={pageHeadHeights[index]}
    inert={index !== activePage}
    class="mx-2.5 shrink-0 @lg:mx-auto @lg:w-[29.5rem] @lg:max-w-full @2xl:w-[37rem]"
  >
    {@render greetingLine(true)}
    {@render headCards()}
  </div>
{/snippet}

{#snippet pageTabs(atBottom: boolean)}
  <!-- The pages. Over the grid they're a pill in the header (see `pill`); on
       a phone they're the tab bar, floating over the blur at the bottom,
       always lifted since content always runs under it, and a step larger
       for a thumb. In the header the first tab's word stands on the
       greeting's line, as Edit's word does on the other side, and the thumb
       reaches past it by the tab's padding: otherwise My apps, unselected,
       starts 14px in from the name and the apps. A phone's canvas keeps the
       thumb on the edge, with no room past it for the lifted pill. -->
  <div
    role="tablist"
    aria-label="Apps"
    tabindex="-1"
    onkeydown={onTabKeydown}
    class={[
      "relative flex items-center p-1",
      atBottom
        ? "ii-elevation-6 pointer-events-auto h-12 rounded-full border border-transparent bg-(--ii-surface-6)"
        : [pill, PILL_MOTION, "-ms-[5px] h-10 @lg:-ms-[19px]"],
    ]}
  >
    <!-- The selected tab sits on a thumb, as in a segmented control, so it
         reads by its shape and not only by its darker text. The thumb is the
         selected overlay, so it shows on the pill at rest, in the canvas's
         colour, and on the lifted one; it slides to a tab picked or swiped
         to. Mounted once the tabs are measured, so it doesn't grow in from
         nothing on load. -->
    {#if thumb !== undefined}
      <span
        aria-hidden="true"
        class="absolute inset-y-1 rounded-full bg-(--ii-selected) transition-[inset-inline-start,width] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none"
        style:inset-inline-start={`${thumb.start}px`}
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

{#snippet accessPreview()}
  {@const methods =
    current.scenario.access === "one"
      ? [PasskeyIcon]
      : [PasskeyIcon, KeyRoundIcon, AtSignIcon]}
  <!-- Up to three of the ways in, as overlapping circles, and how many. Each
       cuts itself out of the card (StatusCard's surface); bg-secondary keeps
       them apart from it in light, where a raised card is white. -->
  <span class="flex items-center">
    {#each methods as Glyph, index (index)}
      <span
        class={[
          "bg-bg-secondary text-fg-tertiary grid size-7 place-items-center rounded-full ring-2 ring-(color:--ii-card-surface)",
          index > 0 && "-ms-2",
        ]}
      >
        <Glyph class="size-3.5" />
      </span>
    {/each}
  </span>
  <span class="text-text-tertiary text-[13px] leading-[18px]">
    {methods.length === 1
      ? "One way to sign in"
      : `${methods.length} ways to sign in`}
  </span>
{/snippet}

{#snippet recoveryPreview()}
  <!-- Both ways back in, each with where it stands. -->
  {#each [{ name: "Phrase", status: phraseStatus(current.scenario.recovery) }, { name: "Email", status: emailStatus(current.scenario.recovery) }] as method (method.name)}
    {@const warn = method.status === "unchecked" || method.status === "pending"}
    <span
      class={[
        "inline-flex items-center gap-1 rounded-full border py-0.5 ps-1.5 pe-2 text-xs font-medium",
        isOn(method.status)
          ? "border-bg-success-secondary bg-bg-success-primary text-fg-success-primary"
          : warn
            ? "text-fg-warning-primary border-transparent bg-(--ii-warning-tint)"
            : "border-border-tertiary bg-bg-primary text-text-tertiary",
      ]}
    >
      <span class="size-1.5 shrink-0 rounded-full bg-current"></span>
      {method.name} · {RECOVERY_SAID[method.status]}
    </span>
  {/each}
{/snippet}

{#snippet devicesPreview()}
  <!-- The browsers signed in, newest marked, as overlapping logos, cut out of
       the card as the ways in are. -->
  <span class="flex items-center">
    {#each browsers.slice(0, 3) as browser, index (browser.id)}
      {@const src = brandIconOf(browser.description)}
      <span
        class={[
          "bg-bg-secondary relative grid size-7 place-items-center rounded-full ring-2 ring-(color:--ii-card-surface)",
          index > 0 && "-ms-2",
        ]}
      >
        {#if src !== undefined}
          <img {src} alt="" class="size-4" />
        {:else}
          <MonitorSmartphoneIcon class="text-fg-tertiary size-3.5" />
        {/if}
        {#if browser.isNew === true}
          <span
            class="bg-fg-error-primary absolute -end-0.5 -top-0.5 size-2 rounded-full ring-2 ring-(color:--ii-card-surface)"
          ></span>
        {/if}
      </span>
    {/each}
  </span>
  <span class="text-text-tertiary text-[13px] leading-[18px]">
    {browsers.length === 1 ? "1 browser" : `${browsers.length} browsers`}
  </span>
{/snippet}

{#snippet addTile()}
  <!-- After the last app, the way to add one by hand: a search over the
       curated apps that also takes a link. While apps are arranged it stays
       where it is as the first free cell: the plus and the label fade, the
       dashed frame stays, and the free cells after it fill out its row. -->
  <button
    type="button"
    onclick={(event) => (addAnchor = event.currentTarget)}
    inert={managing}
    aria-haspopup="dialog"
    aria-expanded={addAnchor !== undefined}
    class="ii-hover focus-visible:ring-focus-ring flex w-full flex-col items-center gap-2.5 rounded-2xl px-1.5 pt-4 pb-3 outline-none focus-visible:ring-2"
  >
    <span
      class="border-border-primary text-fg-tertiary grid size-16 place-items-center rounded-2xl border-[1.5px] border-dashed"
    >
      <span class={["grid", ADD_FADE, managing && "opacity-0"]}>
        <PlusIcon class="size-6" aria-hidden="true" />
      </span>
    </span>
    <span
      class={[
        "text-text-tertiary text-xs leading-tight",
        ADD_FADE,
        managing && "opacity-0",
      ]}
    >
      Add app
    </span>
  </button>
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
      onclick={(event) => (addAnchor = event.currentTarget)}
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
    <!-- The sheet on a phone needs a title, which keeps clear of its Close;
         beside the canvas the identity card heads the panel itself. -->
    {#if inSheet}
      <h2
        class="text-text-primary flex h-12 items-center pe-10 text-base font-semibold"
      >
        Identity
      </h2>
    {/if}
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
    <!-- Three independent cards, each a large target that leads to its
         section: the title, where it stands, and what's inside. -->
    <div class="flex flex-col gap-2.5">
      {#each accountCards as card (card.kind)}
        <Elevated offset={1} class="overflow-hidden rounded-2xl">
          <StatusCard
            kind={card.kind}
            title={TITLES[card.kind]}
            sentence={card.sentence}
            tone={card.tone}
            resolved={resolvedKinds.includes(card.kind)}
            onOpen={() => openCard(card.kind)}
            preview={card.kind === "access"
              ? accessPreview
              : card.kind === "recovery"
                ? recoveryPreview
                : devicesPreview}
          />
        </Elevated>
      {/each}
    </div>
  </div>
{/snippet}

{#snippet panelBody(inSheet: boolean)}
  {#if managing}
    {#if selectedApps.length === 1}
      {@const app = selectedApps[0]}
      <AppView
        {app}
        history={current.keepHistory}
        backLabel="Done"
        onExit={stopManaging}
        onToggleNotifications={(allowed) => setNotifications([app], allowed)}
        onRemove={() => askRemove([app])}
      />
    {:else if selectedApps.length > 1}
      <BulkView
        apps={selectedApps}
        onExit={stopManaging}
        onSetNotifications={(allowed) =>
          setNotifications(selectedApps, allowed)}
        onRemove={() => askRemove(selectedApps)}
      />
    {:else}
      <PanelHeader
        title="Manage apps"
        lead="Select apps to change their permissions or remove them."
        backLabel="Done"
        onBack={stopManaging}
      />
    {/if}
  {:else if openedApp !== undefined}
    {@const app = openedApp}
    <AppView
      {app}
      history={current.keepHistory}
      backLabel={identityName}
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
  {:else if section !== undefined}
    <SectionView
      {section}
      scenario={current.scenario}
      {signedInWith}
      {browsers}
      onBack={closeSection}
      onSignOut={signOut}
      onResolve={resolve}
      onVerifyPhrase={verifyPhrase}
      onUnavailable={unavailable}
    />
  {:else}
    {@render overview(inSheet)}
  {/if}
{/snippet}

{#snippet panel(inSheet: boolean)}
  <!-- Views stack in one cell, so the old one fades while the new one arrives.
       Past the overview, the identity card folds into one line at the top of
       every section, so it's always clear whose access methods, devices or
       apps these are. The line is part of that move: it comes in with the
       first section and leaves with the last, alongside the view, instead of
       growing above the views and pushing both down. Between sections it
       stays put, and only the view under it changes. -->
  <div class="grid">
    {#key viewKey === "overview"}
      <div
        class="col-start-1 row-start-1 min-w-0"
        in:viewIn={{ direction: navDirection * readingDirection(), reduced }}
        out:viewOut={{ reduced }}
      >
        {#if viewKey === "overview"}
          {@render panelBody(inSheet)}
        {:else}
          <div class={[inSheet && "pe-10"]}>
            <Elevated
              offset={1}
              as="header"
              class="mb-4 flex items-center gap-3 rounded-2xl px-3 py-2.5"
            >
              <IdentityAvatar
                {identity}
                palette={identityPaletteOverride}
                size="sm"
              />
              <p class="flex min-w-0 flex-1 flex-col">
                <span class="text-text-primary truncate text-sm font-semibold">
                  {identityName}
                </span>
                <span
                  class="text-text-tertiary truncate text-[13px] leading-[18px]"
                >
                  {signedInWith.title}
                </span>
              </p>
            </Elevated>
          </div>
          <div class="grid">
            {#key viewKey}
              <div
                class="col-start-1 row-start-1 min-w-0"
                in:viewIn={{
                  direction: navDirection * readingDirection(),
                  reduced,
                }}
                out:viewOut={{ reduced }}
              >
                {@render panelBody(inSheet)}
              </div>
            {/key}
          </div>
        {/if}
      </div>
    {/key}
  </div>
{/snippet}

<!-- --ii-edge: a phone's margin, the same on both sides, which the logo,
     the identity, the greeting, the cards, the tabs, the apps and the pages'
     content all stand on; plates (hover, a lifted pill) reach past it. -->
<div
  class="concept ii-surface-1 text-text-primary min-h-dvh [--ii-edge:--spacing(4)] md:flex md:h-dvh md:p-2"
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
      bind:clientHeight={headerHeight}
      class="pointer-events-none relative z-10 flex shrink-0 flex-col"
    >
      <div
        class="grid grid-cols-[1fr_auto] items-center gap-x-1 px-4 sm:px-6 md:h-16 md:grid-cols-[1fr_auto_1fr] md:px-5 @max-lg/canvas:px-(--ii-edge)"
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
            // past it once lifted.
            "@max-lg/canvas:-ms-2.5 @max-lg/canvas:px-[9px]",
          ]}
        >
          <Logo class="text-fg-primary h-4" />
          <span class="text-text-primary text-base font-semibold"> id.ai </span>
        </button>
        <div
          bind:this={controlsPill}
          class={[
            "col-start-2 row-start-1 flex items-center gap-1 justify-self-end md:col-start-3",
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
                  ? "var(--ii-surface-6)"
                  : "var(--ii-surface-3)"}
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
                ? "var(--ii-surface-6)"
                : "var(--ii-surface-3)"}
            >
              {#each accountCards as card (card.kind)}
                {@const Icon = SECTION_ICONS[card.kind]}
                {@const dot = dotOf(card.tone)}
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
      <!-- The greeting and the tabs share the grid's width, so the name, the
           first tab and the first app start on one line, and Edit ends where
           the last column does. -->
      <div class={["mx-auto flex flex-col", GRID_WIDTH]}>
        <!-- The canvas says whose it is before anything else, the way a
             person would: the name, with a dot in the identity's colour
             before it. It stays on every page, since it's true of all of
             them. The name is said here only; the panel's card is for
             switching, and how it signed in is Access's to say. A phone has
             it at the top of each page instead (pageHead). -->
        {#if !phoneHead}
          <div bind:clientHeight={greetingHeight} class="overflow-hidden">
            <div
              class={[
                switchingPage &&
                  "transition-[translate,opacity] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
              ]}
              style:translate={`0 ${-headLift}px`}
              style:opacity={greetingHeight > 0
                ? 1 - headLift / greetingHeight
                : 1}
            >
              {@render greetingLine(false)}
            </div>
          </div>
          <!-- The pages, right above what they show, with Edit across from
               them on My apps, as over a list on iOS (Done while apps are
               arranged). -->
          <div
            class={[
              "flex items-center justify-between gap-3 @lg:pe-6",
              INSET,
              switchingPage &&
                "transition-[translate] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
            ]}
            style:translate={`0 ${-headLift}px`}
          >
            {@render pageTabs(false)}
            {#if activePage === 0 && current.apps.length > 0}
              <button
                type="button"
                onclick={managing ? stopManaging : startManaging}
                in:fade={{ duration: reduced ? 0 : 150 }}
                class={[
                  pill,
                  PILL_MOTION,
                  "ii-hover text-text-secondary hover:text-text-primary focus-visible:outline-focus-ring -me-3.5 h-10 px-3.5 text-sm font-medium focus-visible:outline-2 @max-lg/canvas:-me-2.5 @max-lg/canvas:px-[9px]",
                ]}
              >
                {@render editWords("@max-lg/canvas:justify-items-end")}
              </button>
            {/if}
          </div>
        {/if}
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
          {/if}
          <div
            class="mx-auto flex w-fit max-w-full flex-col @max-lg/canvas:w-full"
          >
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
            {#if current.apps.length > 0}
              <!-- Named by its tab, right above it. -->
              <section aria-labelledby="tab-mine" class="flex flex-col">
                <!-- While apps are arranged, a privacy setting heads the grid.
                     Its room opens as the grid glides down and the card fades
                     in within it; on Done the card fades first and the room
                     closes. It stays mounted, inert while hidden, so a quick
                     Edit and Done turn around midway instead of starting
                     over. It is as wide as the grid (w-0 min-w-full): a long
                     line can't widen the column and shift the grid sideways. -->
                <div
                  inert={!managing}
                  class={[
                    "grid w-0 min-w-full transition-[grid-template-rows] duration-250 ease-(--ii-ease-out) motion-reduce:transition-none",
                    managing ? "grid-rows-[1fr]" : "grid-rows-[0fr]",
                  ]}
                >
                  <div class="min-h-0 overflow-hidden">
                    <label
                      class={[
                        "bg-bg-primary border-border-tertiary mx-2.5 mb-5 flex origin-top items-center gap-4 rounded-2xl border px-4 py-3 transition-[opacity,scale] motion-reduce:scale-100 @lg:mx-2",
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
                  class={GRID}
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
          {/if}
          <!-- Explore reads like the App Store: a banner, a shelf, another
               banner, more shelves. The featured shelf keeps to apps this
               identity doesn't use yet, and steps aside when there are none. -->
          <ExplorePage
            {featured}
            {isAdded}
            onAdd={(app) => addApp(app)}
            onAddLink={addLink}
          />
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
          {/if}
          <ConnectPage
            integrations={current.scenario.integrations}
            apps={current.apps}
            onResolve={resolve}
            onUnavailable={unavailable}
          />
        </div>
      </div>
      <!-- Content comes up from under the island through a blur that deepens
           into the canvas. A page keeps clear of it at rest (PAGE's pb-32). -->
      <ProgressiveBlur side="bottom" fade class="h-24" />
      <!-- Phone: the head's cards go up under the logo whole, so they leave
             through a blur too, rather than at the screen's edge. -->
      {#if phoneHead}
        <ProgressiveBlur side="top" fade class="h-(--header-h)" />
      {/if}
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
        // starts 21px down, on the top of the canvas header's pills (8px page
        // padding, the frame's 1px, then 12px).
        "flex h-full w-[25rem] flex-col overflow-y-auto overscroll-contain ps-6 pe-4 pt-[13px] pb-4",
        "transition-[opacity,translate] duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
        !panelVisible && "translate-x-4 opacity-0 rtl:-translate-x-4",
      ]}
    >
      {#if isDesktop.current}
        <!-- The panel lies on the page, but its cards stand level with the
             canvas (3): a step lower, beside it, they read as sunk into the
             page. So all it holds counts as standing on 2. -->
        <div class="shrink-0">
          <SurfaceLevel level={2}>
            {@render panel(false)}
          </SurfaceLevel>
        </div>
        <!-- Concept only, to try both themes: it keeps to the panel's foot, at
             the bottom of the window while the panel is short, and after the
             last card once it is long. Two glyphs on a track, the one in use
             raised, as a footer's theme switch is on Vercel's pages. -->
        <div class="mt-auto flex shrink-0 items-center gap-3 ps-4 pt-6">
          <span
            id="concept-theme-label"
            class="text-text-tertiary flex-1 text-[13px] leading-[18px] font-medium"
          >
            Theme
          </span>
          <div
            role="radiogroup"
            aria-labelledby="concept-theme-label"
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
                    ? "text-fg-primary bg-(--ii-surface-6) shadow-(--ii-shadow-2)"
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
  {#if !isDesktop.current && incident !== undefined && !incidentDismissed && !panelOpen && verifying === undefined}
    <Dialog onClose={() => (incidentDismissed = true)}>
      <IncidentCard {incident} variant="sheet" onAnswer={answerIncident} />
    </Dialog>
  {/if}

  <!-- Phone: the panel is a sheet from the bottom, as dialogs are elsewhere. -->
  {#if panelOpen && !isDesktop.current}
    <Dialog onClose={() => (panelOpen = false)}>
      {@render panel(true)}
    </Dialog>
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

  {#if addAnchor !== undefined}
    <!-- Rendered here, outside the canvas, but it opens over the canvas. -->
    <Popup
      over={3}
      anchor={addAnchor}
      onClose={() => (addAnchor = undefined)}
      direction="down"
      align="center"
      distance="0.5rem"
      class="!w-[min(26rem,calc(100vw-2rem))]"
    >
      <div class="flex max-h-[min(32rem,70dvh)] flex-col overflow-y-auto p-3">
        <AppSearch
          autofocus
          suggestions={featured.slice(0, 4)}
          {isAdded}
          onAdd={(app) => addApp(app)}
          onAddLink={addLink}
        />
        <button
          type="button"
          onclick={() => {
            addAnchor = undefined;
            goToPage(1);
          }}
          class="ii-hover text-text-primary focus-visible:ring-focus-ring mt-2 flex items-center gap-2.5 rounded-xl px-3 py-2.5 text-start text-sm font-medium outline-none focus-visible:ring-2 focus-visible:ring-inset"
        >
          <CompassIcon class="text-fg-quaternary size-4" aria-hidden="true" />
          Browse Explore
        </button>
      </div>
    </Popup>
  {/if}

  {#if recoveryOpen}
    <RecoveryFlow
      recovery={current.scenario.recovery}
      {identityName}
      focus={recoveryFocus}
      onChange={resolve}
      onClose={() => (recoveryOpen = false)}
    />
  {/if}

  {#if removing !== undefined}
    {@const apps = removing}
    {@const what = apps.length === 1 ? apps[0].name : `${apps.length} apps`}
    <Dialog onClose={() => (removing = undefined)}>
      <div class="flex flex-col gap-5">
        <h2 class="text-text-primary text-lg font-semibold">
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
    </Dialog>
  {/if}

  {#if verifying !== undefined}
    <VerifyPhraseDialog
      onVerified={passVerify}
      onClose={() => (verifying = undefined)}
    />
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
      </div>
    </Popup>
  {/if}
</div>
