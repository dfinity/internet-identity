<script lang="ts">
  import "./surfaces.css";
  import {
    ArrowRightToLineIcon,
    AtSignIcon,
    BellOffIcon,
    CheckIcon,
    ChevronRightIcon,
    CompassIcon,
    FlaskConicalIcon,
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
    identityPaletteIndex,
  } from "$lib/utils/identityColors";
  import ActionDock, { type DockItem } from "./components/ActionDock.svelte";
  import AppTile from "./components/AppTile.svelte";
  import AttentionDot from "./components/AttentionDot.svelte";
  import AppView from "./components/AppView.svelte";
  import BulkView from "./components/BulkView.svelte";
  import ConnectPage from "./components/ConnectPage.svelte";
  import CustomizeIdentityDialog from "./components/CustomizeIdentityDialog.svelte";
  import Elevated from "./components/Elevated.svelte";
  import Popup from "./components/Popup.svelte";
  import AppRow from "./components/AppRow.svelte";
  import AppSearch from "./components/AppSearch.svelte";
  import ExplorePage from "./components/ExplorePage.svelte";
  import IdentityCard from "./components/IdentityCard.svelte";
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
  import StatusRow from "./components/StatusRow.svelte";
  import VerifyPhraseDialog from "./components/VerifyPhraseDialog.svelte";
  import {
    browsersFor,
    featuredApps,
    identities,
    scenarioPresets,
    type ConceptApp,
    type FeaturedApp,
  } from "./mockData";
  import { viewIn, viewOut } from "./motion";
  import { ConceptTheme } from "./theme.svelte";
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

  /** Names and colours picked in Customize, per identity. The name would be
   *  saved on the canister (identity_properties_replace takes one); where a
   *  picked colour lives is still open. */
  const customized = $state<
    Record<string, { name: string; paletteIndex: number }>
  >({});
  let customizing = $state(false);
  const identity = $derived.by(() => {
    const base = identities[identityIndex].identity;
    const own = customized[`${base.identityNumber}`];
    return own === undefined ? base : { ...base, name: own.name };
  });
  const identityPaletteOverride = $derived.by(() => {
    const own = customized[`${identity.identityNumber}`];
    return own === undefined ? undefined : IDENTITY_PALETTES[own.paletteIndex];
  });
  const identityName = $derived(identity.name ?? `${identity.identityNumber}`);
  const signedInWith = $derived(identities[identityIndex].signedInWith);
  const current = $derived(states[identityIndex]);

  /** The app whose details the panel shows: a tile's plain click opens them
   *  beside the grid (in the sheet on a phone). */
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
  /** The account's rows, in one untitled group: how you get in, then where
   *  the identity is signed in. Apps are managed from the grid itself
   *  (Manage, by All apps' heading), so the account doesn't list them too. */
  const ACCOUNT_KINDS: CardKind[] = ["access", "recovery", "devices"];
  const accountCards = $derived(
    ACCOUNT_KINDS.map((kind) =>
      cards.find((card) => card.kind === kind),
    ).filter((card) => card !== undefined),
  );
  /** What the calm island mentions: something new or waiting, never a tip. */
  const note = $derived(
    cards.find(({ tone }) =>
      (["queued", "pending", "now", "recent"] as const).some((t) => t === tone),
    ),
  );

  // --- Apps ---

  // One grid, in the order apps were added (or arranged): a newly used app
  // goes to the end, so nothing moves by itself.
  /** The tile after the last app, which adds one. */
  const ADD_TILE = { origin: "add-app" } as const;
  type GridItem = ConceptApp | typeof ADD_TILE;
  const gridItems = $derived.by<GridItem[]>(() =>
    managing ? current.apps : [...current.apps, ADD_TILE],
  );
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
  const PAGE =
    "flex w-full shrink-0 snap-start snap-always flex-col overflow-y-auto overscroll-y-contain px-2 pt-[calc(var(--header-h)+3rem)] pb-32 scroll-pt-[calc(var(--header-h)+3rem)] [scrollbar-color:transparent_transparent] [scrollbar-gutter:stable_both-edges] [scrollbar-width:thin] sm:px-6";
  /** A page at rest: its thumb fades in. It hides at once when a swipe
   *  starts, as the transition is only here. */
  const SETTLED =
    "[scrollbar-color:var(--color-fg-quaternary)_transparent] transition-[scrollbar-color] duration-200 ease-out motion-reduce:transition-none";
  /** The header's height: the pages run up under it. */
  let headerHeight = $state(64);

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

  /** A tile's plain click: its details take the panel, or the sheet. */
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

  // The header is a row of pills in the canvas's own colour, so at rest they
  // don't show. Once a page's content reaches the header they lift off the
  // canvas, as the dock's pills sit over it, and keep the header readable
  // over whatever runs under it.
  /** The scroll at which a page's content reaches the header: the 3rem PAGE
   *  keeps under it. */
  const UNDER_HEADER_AT = 48;
  const underHeader = $state(PAGES.map(() => false));
  const headerLifted = $derived(underHeader[activePage]);
  /** The pages scroll on their own; the pager hears them while capturing. */
  const onPageScroll = (event: Event) => {
    const index = pageRefs.indexOf(event.target as HTMLElement);
    if (index !== -1) {
      underHeader[index] = pageRefs[index].scrollTop > UNDER_HEADER_AT;
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
      switching = false;
      appOpen = undefined;
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
    onSelect={() => openAppDetails(app)}
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
  <!-- Up to three of the ways in, as overlapping circles. -->
  <span class="flex items-center">
    {#each current.scenario.access === "one" ? [PasskeyIcon] : [PasskeyIcon, KeyRoundIcon, AtSignIcon] as Glyph, index (index)}
      <span
        class={[
          "bg-bg-primary text-fg-tertiary grid size-7 place-items-center rounded-full ring-2 ring-(color:--ii-surface-2)",
          index > 0 && "-ms-2",
        ]}
      >
        <Glyph class="size-3.5" />
      </span>
    {/each}
  </span>
{/snippet}

{#snippet devicesPreview()}
  <!-- The browsers signed in, newest marked, as overlapping logos. -->
  <span class="flex items-center">
    {#each browsers.slice(0, 3) as browser, index (browser.id)}
      {@const src = brandIconOf(browser.description)}
      <span
        class={[
          "bg-bg-primary relative grid size-7 place-items-center rounded-full ring-2 ring-(color:--ii-surface-2)",
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
            class="bg-fg-error-primary absolute -end-0.5 -top-0.5 size-2 rounded-full ring-2 ring-(color:--ii-surface-2)"
          ></span>
        {/if}
      </span>
    {/each}
  </span>
{/snippet}

{#snippet addTile()}
  <!-- After the last app, the way to add one by hand: a search over the
       curated apps that also takes a link. -->
  <button
    type="button"
    onclick={(event) => (addAnchor = event.currentTarget)}
    aria-haspopup="dialog"
    aria-expanded={addAnchor !== undefined}
    class="ii-hover focus-visible:ring-focus-ring flex w-full flex-col items-center gap-2.5 rounded-2xl px-1.5 pt-4 pb-3 outline-none focus-visible:ring-2"
  >
    <span
      class="border-border-primary text-fg-tertiary grid size-16 place-items-center rounded-2xl border-[1.5px] border-dashed"
    >
      <PlusIcon class="size-6" aria-hidden="true" />
    </span>
    <span class="text-text-tertiary text-xs leading-tight">Add app</span>
  </button>
{/snippet}

{#snippet firstApps()}
  <!-- No apps yet: a calm panel that says what will be here, with the one
       action inside it. -->
  <div
    class="border-border-tertiary flex w-[min(36rem,calc(100vw-2rem))] flex-col items-center gap-2 rounded-3xl border bg-(--ii-hover) px-6 py-9 text-center"
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
  <section class="mt-10 flex w-[min(36rem,calc(100vw-2rem))] flex-col gap-2">
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
      palette={identityPaletteOverride}
      onSwitch={switchIdentity}
      onAdd={() => unavailable("Adding an identity")}
      onCustomize={() => (customizing = true)}
      onStartPage={() =>
        toaster.create({
          title: "Make id.ai your start page",
          description:
            "In your browser's settings, under “On startup”, open id.ai.",
          duration: 5000,
        })}
      onSignOut={() => unavailable("Signing out")}
    >
      {#snippet alert()}
        {#if incident !== undefined}
          <div out:slide={{ duration: reduced ? 0 : 260, easing: quintOut }}>
            <IncidentCard {incident} onAnswer={answerIncident} />
          </div>
        {/if}
      {/snippet}
    </IdentityCard>
    <!-- One card of rows, as a group in Settings on iOS, with no heading:
         the rows' titles say what each is. -->
    <Elevated
      offset={1}
      as="ul"
      aria-label="Account"
      class="ii-rows flex flex-col overflow-hidden rounded-2xl [--ii-rows-inset:4rem]"
    >
      {#each accountCards as card (card.kind)}
        <StatusRow
          kind={card.kind}
          title={TITLES[card.kind]}
          sentence={card.sentence}
          tone={card.tone}
          action={isLoud(card) && !coveredByIncident(card.kind)
            ? leadActionOf(card, current.scenario)
            : undefined}
          resolved={resolvedKinds.includes(card.kind)}
          onOpen={() => openCard(card.kind)}
          onAction={() => runLead(card)}
          preview={card.kind === "access"
            ? accessPreview
            : card.kind === "devices"
              ? devicesPreview
              : undefined}
        />
      {/each}
    </Elevated>
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
  <!-- Past the overview, the identity card folds into one line that stays at
       the top through every section, so it's always clear whose access
       methods, devices or apps these are. It stays put while the views below
       it change. -->
  {#if viewKey !== "overview"}
    <div
      transition:slide={{ duration: reduced ? 0 : 240, easing: quintOut }}
      class={[inSheet && "pe-10"]}
    >
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
          <span class="text-text-tertiary truncate text-[13px] leading-[18px]">
            {signedInWith.title}
          </span>
        </p>
      </Elevated>
    </div>
  {/if}
  <!-- Views stack in one cell, so the old one fades while the new one arrives. -->
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

<div
  class="concept ii-surface-1 text-text-primary min-h-dvh md:flex md:h-dvh md:p-2"
>
  <!-- The canvas: the apps, in a frame on the page. It doesn't scroll: the
       header stays put and each page scrolls on its own under it. -->
  <Elevated
    offset={2}
    as="main"
    class="@container flex h-dvh min-w-0 flex-1 flex-col overflow-hidden max-md:rounded-none max-md:border-0 max-md:shadow-none md:h-auto md:min-h-0 md:rounded-[1.25rem]"
  >
    <div class="h-[env(safe-area-inset-top)] shrink-0"></div>
    <h1 class="sr-only">Apps</h1>

    <!-- On desktop one row: the logo, the pages centered on the canvas, and an
         empty column that keeps them centered. A phone has no room for the
         pages beside its controls, so they drop to a row of their own. Each
         is a pill (see `pill`); between them the pages show through. From
         `md` the inset is 20px, where the account's pill ends. -->
    <header
      bind:clientHeight={headerHeight}
      class="pointer-events-none relative z-10 grid shrink-0 grid-cols-[1fr_auto] items-center gap-x-1 px-4 sm:px-6 md:h-16 md:grid-cols-[1fr_auto_1fr] md:px-5"
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
        ]}
      >
        <Logo class="text-fg-primary h-4" />
        <span class="text-text-primary text-base font-semibold"> id.ai </span>
      </button>
      <div
        role="tablist"
        aria-label="Apps"
        tabindex="-1"
        onkeydown={onTabKeydown}
        class={[
          pill,
          PILL_MOTION,
          "relative col-span-2 row-start-2 mt-2 flex h-10 items-center justify-self-center p-1 md:col-span-1 md:col-start-2 md:row-start-1 md:mt-0",
        ]}
      >
        <!-- The selected tab sits on a thumb, as in a segmented control, so it
             reads by its shape and not only by its darker text. The thumb is
             the selected overlay, so it shows on the pill at rest, in the
             canvas's colour, and on the lifted one; it slides to a tab picked
             or swiped to. Mounted once the tabs are measured, so it doesn't
             grow in from nothing on load. -->
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
              "focus-visible:outline-focus-ring relative h-8 rounded-full px-3.5 text-sm font-semibold transition-colors duration-150 ease-out focus-visible:outline-2 motion-reduce:transition-none",
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
        bind:this={controlsPill}
        class={[
          "col-start-2 row-start-1 flex items-center gap-1 justify-self-end md:col-start-3",
          layout === "panel" && "md:hidden",
          // A phone's controls share one pill; on desktop the account, or
          // Close, has its own.
          !isDesktop.current && [pill, PILL_MOTION, "h-10 px-[3px]"],
        ]}
      >
        <!-- Phone only: on desktop it keeps to the bottom-left corner. -->
        <div class="flex md:hidden">
          {@render conceptControls(true)}
        </div>
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
        {#if layout === "panel"}
          <!-- Phone: the panel opens from here; the dot says it has something for you. -->
          <button
            type="button"
            onclick={() => (panelOpen = true)}
            aria-label={`Open ${identityName}${needsAttention(cards) ? ", needs your attention" : ""}`}
            class="relative -me-2 flex size-11 items-center justify-center rounded-full md:hidden"
          >
            <IdentityAvatar
              {identity}
              palette={identityPaletteOverride}
              size="sm"
            />
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
          <div class="mx-auto flex w-fit max-w-full flex-col md:my-auto">
            <!-- The page says whose it is before anything else: the identity's
                 own avatar and name, said the way a person would. -->
            {#if !managing}
              <div
                class={[
                  "mb-8 flex items-center gap-3.5 lg:mb-10",
                  INSET,
                  "lg:@lg:ps-5",
                ]}
              >
                <IdentityAvatar
                  {identity}
                  palette={identityPaletteOverride}
                  size="lg"
                />
                <div class="flex min-w-0 flex-col">
                  <p
                    class="text-text-primary truncate text-xl leading-7 font-semibold tracking-tight"
                  >
                    {greeting}, {identityName}
                  </p>
                  <p class="text-text-tertiary text-sm">
                    Signed in with {signedInWith.title}
                  </p>
                </div>
              </div>
            {/if}
            <!-- Phone: an incident put away unanswered heads the apps. -->
            {#if !isDesktop.current && incident !== undefined && !managing}
              <div
                transition:slide={{
                  duration: reduced ? 0 : 260,
                  easing: quintOut,
                }}
                class="-mx-1.5 w-0 min-w-[calc(100%+0.75rem)] pb-8 @lg:-mx-2 @lg:min-w-[calc(100%+1rem)]"
              >
                <IncidentBanner
                  {incident}
                  class="w-full"
                  onOpen={() => (incidentDismissed = false)}
                />
              </div>
            {/if}
            {#if current.apps.length > 0}
              <section class="flex flex-col gap-2">
                <!-- Edit stands at the heading's other end, as over a list on
                     iOS, and Done while apps are arranged. -->
                <div
                  class={[
                    "flex items-baseline justify-between gap-4 pe-2.5 @lg:pe-6",
                    INSET,
                  ]}
                >
                  <h2 class="text-text-tertiary text-sm font-medium">
                    My apps
                  </h2>
                  <button
                    type="button"
                    onclick={managing ? stopManaging : startManaging}
                    class="text-text-secondary hover:text-text-primary focus-visible:outline-focus-ring -mx-1.5 rounded-md px-1.5 text-sm font-medium transition-colors duration-150 focus-visible:outline-2"
                  >
                    {managing ? "Done" : "Edit"}
                  </button>
                </div>
                {#if managing}
                  <label
                    transition:slide={{
                      duration: reduced ? 0 : 220,
                      easing: quintOut,
                    }}
                    class="bg-bg-primary border-border-tertiary mx-1.5 mb-3 flex items-center gap-4 rounded-2xl border px-4 py-3 @lg:mx-2"
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
                {/if}
                <ReorderGrid
                  items={gridItems}
                  key={(item) => item.origin}
                  editing={managing}
                  onMove={moveApp}
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
        <div class="shrink-0">
          {@render panel(false)}
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

  {#if customizing}
    <CustomizeIdentityDialog
      name={identityName}
      paletteIndex={customized[`${identity.identityNumber}`]?.paletteIndex ??
        identityPaletteIndex(identity.identityNumber)}
      signedInWith={signedInWith.title}
      onSave={(value) => {
        customized[`${identity.identityNumber}`] = value;
        customizing = false;
      }}
      onClose={() => (customizing = false)}
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
       On desktop it stays in the bottom-left corner in every layout; on a
       phone it sits in the header, leaving the bottom to the island. -->
  <div class="fixed start-4 bottom-4 z-10 max-md:hidden">
    {@render conceptControls(false)}
  </div>

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
