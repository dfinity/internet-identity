/**
 * Mock data for the holistic-redesign UI concept. Nothing here talks to a canister.
 *
 * Shapes follow what the canister already returns where it can (the apps an identity
 * signed in to, the browsers it is signed in on), and are marked where the concept
 * needs data Internet Identity does not hold today.
 */
import type { BrowserDescription } from "$lib/generated/internet_identity_types";
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import dappsJson from "$lib/legacy/flows/dappsExplorer/dapps.json" with { type: "json" };
import { KnownDapp } from "$lib/legacy/flows/dappsExplorer/dapps";
import type { Scenario } from "./status";

const MINUTE = 60 * 1000;
const HOUR = 60 * MINUTE;
const DAY = 24 * HOUR;

const now = Date.now();

const dapp = (name: string): KnownDapp => {
  const descr = dappsJson.find((entry) => entry.name === name);
  if (descr === undefined) {
    throw new Error(`No curated dapp named ${name}`);
  }
  return new KnownDapp(descr);
};

export interface MockNotification {
  id: string;
  title: string;
  body: string;
  sentMillis: number;
}

export interface ConceptApp {
  /** The origin the app's identity is derived for. */
  origin: string;
  name: string;
  logo: string;
  url: string;
  /** From `list_applications`: the latest sign-in to the app. */
  lastUsedMillis: number;
  /** From `list_applications`: whether the identity allows the app to notify it. */
  notificationsAllowed: boolean;
  /** Not in Internet Identity today: the browser the app was last signed in from. */
  lastBrowser: string;
  /** Not in Internet Identity today: when an AI agent last used the app. */
  aiUsedMillis?: number;
  /** Not in Internet Identity today: notifications the identity has not read. Their
   *  text lives in the app's canister, and the inbox is per browser. */
  unread: MockNotification[];
}

export interface FeaturedApp {
  origin: string;
  name: string;
  logo: string;
  url: string;
  oneLiner?: string;
}

const app = (
  name: string,
  rest: Omit<ConceptApp, "origin" | "name" | "logo" | "url">,
): ConceptApp => {
  const known = dapp(name);
  return {
    origin: new URL(known.website).origin,
    name: known.name,
    logo: known.logoSrc,
    url: known.website,
    ...rest,
  };
};

/** `oneLiner` stands in where the curated list has none. */
const featured = (name: string, oneLiner?: string): FeaturedApp => {
  const known = dapp(name);
  return {
    origin: new URL(known.website).origin,
    name: known.name,
    logo: known.logoSrc,
    url: known.website,
    oneLiner: known.oneLiner ?? oneLiner,
  };
};

const notification = (
  id: string,
  title: string,
  body: string,
  agoMillis: number,
): MockNotification => ({ id, title, body, sentMillis: now - agoMillis });

const thomasApps: ConceptApp[] = [
  app("OISY Wallet", {
    lastUsedMillis: now - 12 * MINUTE,
    notificationsAllowed: true,
    lastBrowser: "Chrome on Mac",
    unread: [
      notification(
        "oisy-1",
        "Transfer received",
        "12.5 ICP arrived from a saved contact.",
        10 * MINUTE,
      ),
      notification(
        "oisy-2",
        "Swap completed",
        "Your ckBTC to ICP swap went through.",
        HOUR,
      ),
      notification(
        "oisy-3",
        "Weekly summary",
        "Your activity for this week is ready to view.",
        2 * HOUR,
      ),
    ],
  }),
  app("OpenChat", {
    lastUsedMillis: now - 40 * MINUTE,
    notificationsAllowed: true,
    lastBrowser: "Safari on iPhone",
    aiUsedMillis: now - 5 * MINUTE,
    unread: [
      notification(
        "oc-1",
        "New message",
        "Sam: Are we still on for tomorrow?",
        25 * MINUTE,
      ),
    ],
  }),
  app("NNS Dapp", {
    lastUsedMillis: now - 3 * HOUR,
    notificationsAllowed: true,
    lastBrowser: "Chrome on Mac",
    unread: [
      notification(
        "nns-1",
        "Vote now",
        "A proposal you follow is open until Friday.",
        3 * HOUR,
      ),
    ],
  }),
  app("KongSwap", {
    lastUsedMillis: now - DAY,
    notificationsAllowed: false,
    lastBrowser: "Chrome on Mac",
    unread: [],
  }),
  app("DSCVR", {
    lastUsedMillis: now - 2 * DAY,
    notificationsAllowed: true,
    lastBrowser: "Firefox on Windows",
    unread: [],
  }),
  app("TAGGR", {
    lastUsedMillis: now - 4 * DAY,
    notificationsAllowed: false,
    lastBrowser: "Safari on iPhone",
    unread: [],
  }),
  app("Water Neuron", {
    lastUsedMillis: now - 9 * DAY,
    notificationsAllowed: true,
    lastBrowser: "Chrome on Mac",
    aiUsedMillis: now - 2 * DAY,
    unread: [],
  }),
  app("ICPSwap", {
    lastUsedMillis: now - 16 * DAY,
    notificationsAllowed: false,
    lastBrowser: "Chrome on Mac",
    unread: [],
  }),
  app("Juno", {
    lastUsedMillis: now - 30 * DAY,
    notificationsAllowed: false,
    lastBrowser: "Firefox on Windows",
    unread: [],
  }),
  app("Funded", {
    lastUsedMillis: now - 64 * DAY,
    notificationsAllowed: false,
    lastBrowser: "Safari on iPhone",
    unread: [],
  }),
];

const workApps: ConceptApp[] = [
  app("Juno", {
    lastUsedMillis: now - 2 * HOUR,
    notificationsAllowed: true,
    lastBrowser: "Chrome on Mac",
    aiUsedMillis: now - 30 * MINUTE,
    unread: [
      notification(
        "juno-1",
        "Deploy finished",
        "Your satellite is live with the latest build.",
        2 * HOUR,
      ),
    ],
  }),
  app("NNS Dapp", {
    lastUsedMillis: now - 3 * DAY,
    notificationsAllowed: false,
    lastBrowser: "Chrome on Mac",
    unread: [],
  }),
];

/** What the current home page features, as configured per deployment. */
export const featuredApps: FeaturedApp[] = [
  featured("OISY Wallet"),
  featured("OpenChat"),
  featured("NNS Dapp"),
  featured("DecideAI"),
  featured("Yuku"),
  featured("Catalyze"),
];

export interface ExploreBannerItem {
  kind: "banner";
  app: FeaturedApp;
  eyebrow: string;
  title: string;
  /** The card's background, as CSS. */
  art: string;
  /** Light text on a dark card. */
  dark: boolean;
}

export interface ExploreShelfItem {
  kind: "shelf";
  title: string;
  apps: FeaturedApp[];
}

/** Where the featured apps the identity doesn't use yet go in the feed. */
export interface ExploreFeaturedItem {
  kind: "featured";
  title: string;
}

/** Not in Internet Identity today: Explore laid out the way the App Store
 *  lays out its Today and Apps tabs, a banner, a shelf, another banner, more
 *  shelves. Apps and their one-liners come from the curated dapps list. */
export const exploreFeed: (
  ExploreBannerItem | ExploreShelfItem | ExploreFeaturedItem
)[] = [
  {
    kind: "banner",
    app: featured("Caffeine"),
    eyebrow: "Build with AI",
    title: "Describe an app. Caffeine builds it.",
    art: "radial-gradient(90% 130% at 80% 45%, #6b3416 0%, #2a130a 45%, #0c0b0b 100%)",
    dark: true,
  },
  { kind: "featured", title: "Featured on Internet Identity" },
  {
    kind: "banner",
    app: featured("Open Cloud"),
    eyebrow: "Sovereign cloud",
    title: "A cloud of your own, on the Internet Computer.",
    art: "radial-gradient(90% 130% at 80% 40%, #ffffff 0%, #dbe7ff 40%, #b3c9f7 100%)",
    dark: false,
  },
  {
    kind: "shelf",
    title: "Trade and earn",
    apps: [
      featured(
        "ICPSwap",
        "Swap and provide liquidity on the Internet Computer",
      ),
      featured("Sonic DEX"),
      featured("ICPEx"),
      featured("Water Neuron"),
      featured("NeuronPool"),
      featured("BIT10"),
    ],
  },
  {
    kind: "shelf",
    title: "Talk and share",
    apps: [
      featured("distrikt"),
      featured("Hobbi"),
      featured("MORA"),
      featured("Dmail Network"),
      featured("Signals"),
      featured("RuBaRu"),
    ],
  },
  {
    kind: "shelf",
    title: "Play",
    apps: [
      featured("dragginz"),
      featured("Cosmicrafts"),
      featured("AutoRoyale"),
    ],
  },
];

export interface MockIdentity {
  identity: LastUsedIdentity;
  /** The access method this browser signed in with. */
  signedInWith: { title: string; meta: string; kind: "passkey" | "sso" };
  apps: ConceptApp[];
  scenario: Scenario;
}

export const identities: MockIdentity[] = [
  {
    identity: {
      identityNumber: BigInt(10_042),
      name: "Thomas",
      authMethod: { passkey: { credentialId: new Uint8Array([1]) } },
      lastUsedTimestampMillis: now,
    },
    signedInWith: {
      title: "iCloud Keychain",
      meta: "Synced across your Apple devices",
      kind: "passkey",
    },
    apps: thomasApps,
    scenario: {
      recovery: "unverified",
      access: "calm",
      devices: "newBrowser",
      integrations: "calm",
    },
  },
  {
    identity: {
      identityNumber: BigInt(10_077),
      name: "Work",
      authMethod: { sso: { domain: "dfinity.org", name: "DFINITY" } },
      lastUsedTimestampMillis: now - DAY,
    },
    signedInWith: {
      title: "DFINITY SSO",
      meta: "Your organization's sign-in",
      kind: "sso",
    },
    apps: workApps,
    scenario: {
      recovery: "calm",
      access: "calm",
      devices: "calm",
      integrations: "local",
    },
  },
  {
    identity: {
      identityNumber: BigInt(10_103),
      name: "Testing",
      authMethod: { passkey: { credentialId: new Uint8Array([2]) } },
      lastUsedTimestampMillis: now - 5 * DAY,
    },
    signedInWith: {
      title: "Google Password Manager",
      meta: "Synced across your Android devices",
      kind: "passkey",
    },
    apps: [],
    scenario: {
      recovery: "none",
      access: "one",
      devices: "calm",
      integrations: "off",
    },
  },
];

/** Presets for the concept's scenario picker, so each card state can be reviewed. */
export const scenarioPresets: { label: string; scenario: Scenario }[] = [
  {
    label: "Phrase not verified, new browser",
    scenario: identities[0].scenario,
  },
  {
    label: "All set up",
    scenario: {
      recovery: "calm",
      access: "calm",
      devices: "calm",
      integrations: "calm",
    },
  },
  {
    label: "No recovery, one passkey",
    scenario: {
      recovery: "none",
      access: "one",
      devices: "calm",
      integrations: "off",
    },
  },
  {
    label: "Signed in with recovery phrase",
    scenario: {
      recovery: "usedPhrase",
      access: "calm",
      devices: "calm",
      integrations: "calm",
    },
  },
  {
    label: "Phrase reset from a new browser",
    scenario: {
      recovery: "phraseReset",
      access: "newPasskey",
      devices: "newBrowser",
      integrations: "calm",
    },
  },
  {
    label: "Email recovery pending",
    scenario: {
      recovery: "emailPending",
      access: "unused",
      devices: "many",
      integrations: "cliConfirm",
    },
  },
  {
    label: "Local MCP server open",
    scenario: {
      recovery: "calm",
      access: "calm",
      devices: "calm",
      integrations: "local",
    },
  },
];

export interface MockBrowser {
  id: string;
  description: BrowserDescription;
  lastUsedMillis: number;
  isCurrent?: boolean;
  isNew?: boolean;
  /** Unused long enough to suggest signing it out. */
  isStale?: boolean;
}

const browser = (
  id: string,
  brand: BrowserDescription["brand"],
  os: BrowserDescription["os"],
  formFactor: BrowserDescription["form_factor"],
  lastUsedMillis: number,
  extra: Partial<MockBrowser> = {},
): MockBrowser => ({
  id,
  description: { brand, os, form_factor: formFactor, model: [] },
  lastUsedMillis,
  ...extra,
});

/** The browsers behind the Devices card, for its state. */
export const browsersFor = (devices: Scenario["devices"]): MockBrowser[] => {
  const list: MockBrowser[] = [
    browser("this", { Chrome: null }, { Macos: null }, { Desktop: null }, now, {
      isCurrent: true,
    }),
    browser(
      "iphone",
      { Safari: null },
      { Ios: null },
      { Mobile: null },
      now - DAY,
    ),
  ];
  if (devices === "newBrowser") {
    list.push(
      browser(
        "firefox",
        { Firefox: null },
        { Windows: null },
        { Desktop: null },
        now - 10 * MINUTE,
        { isNew: true },
      ),
    );
  }
  if (devices === "many") {
    list.push(
      browser(
        "edge",
        { Edge: null },
        { Windows: null },
        { Desktop: null },
        now - 150 * DAY,
        { isStale: true },
      ),
      browser(
        "samsung",
        { SamsungInternet: null },
        { Android: null },
        { Mobile: null },
        now - 210 * DAY,
        { isStale: true },
      ),
      browser(
        "linux",
        { Firefox: null },
        { Linux: null },
        { Desktop: null },
        now - 300 * DAY,
        { isStale: true },
      ),
      browser(
        "ipad",
        { Safari: null },
        { Ipados: null },
        { Tablet: null },
        now - 7 * DAY,
      ),
    );
  }
  return list;
};
