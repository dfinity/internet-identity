/**
 * What each card in the identity panel says, and how loudly.
 *
 * Three rules, carried over from the team's prototype:
 * 1. One situation is told once, by the card that can resolve it.
 * 2. During an ongoing situation or a real risk, only the cards that resolve it stay
 *    loud; every other to-do drops to a tip.
 * 3. Only one card asks for action; further to-dos wait quietly behind it.
 */

export type RecoveryState =
  | "none"
  | "unverified"
  | "usedPhrase"
  | "emailPending"
  | "phraseReset"
  | "noEmail"
  | "emailOnly"
  | "calm";
/** Where each way back in stands, as its row says it. */
export type MethodStatus =
  "off" | "unchecked" | "pending" | "on" | "inUse" | "new";

export const phraseStatus = (state: RecoveryState): MethodStatus =>
  state === "none" || state === "emailOnly"
    ? "off"
    : state === "unverified"
      ? "unchecked"
      : state === "usedPhrase"
        ? "inUse"
        : state === "phraseReset"
          ? "new"
          : "on";

export const emailStatus = (state: RecoveryState): MethodStatus =>
  state === "emailPending"
    ? "pending"
    : state === "none" || state === "unverified" || state === "noEmail"
      ? "off"
      : "on";

export const isOn = (status: MethodStatus) =>
  status === "on" || status === "inUse" || status === "new";

export type AccessState = "one" | "newPasskey" | "unused" | "calm";
/** `known`: the new browser stays, once the user said it was theirs. */
export type DevicesState = "newBrowser" | "known" | "many" | "calm";
/** `calm`: AI and CLI access both on. `aiOnly` and `cliOnly`: one of them.
 *  `local`: AI access trusts a local server too. `localAllowed`: it stays,
 *  once the user said they set it up. `cliConfirm`: AI access on, and the
 *  ICP CLI waits for approval to sign in. */
export type IntegrationsState =
  | "local"
  | "localAllowed"
  | "cliConfirm"
  | "aiOnly"
  | "cliOnly"
  | "off"
  | "calm";

export const aiAccessOn = (state: IntegrationsState): boolean =>
  state !== "off" && state !== "cliOnly";

/** A CLI that waits for approval isn't on yet. */
export const cliAccessOn = (state: IntegrationsState): boolean =>
  state !== "off" && state !== "aiOnly" && state !== "cliConfirm";

/** The state once AI access is switched, or `undefined` where the concept has
 *  no state for the result. Switching it off forgets a local server too, as
 *  the product does. */
export function withAiAccess(
  state: IntegrationsState,
  on: true,
): IntegrationsState;
export function withAiAccess(
  state: IntegrationsState,
  on: boolean,
): IntegrationsState | undefined;
export function withAiAccess(
  state: IntegrationsState,
  on: boolean,
): IntegrationsState | undefined {
  if (aiAccessOn(state) === on) return state;
  if (on) return cliAccessOn(state) ? "calm" : "aiOnly";
  if (state === "cliConfirm") return undefined;
  return cliAccessOn(state) ? "cliOnly" : "off";
}

/** The state once CLI access is switched, or `undefined` where the concept has
 *  no state for the result. On approves a CLI that waits; off declines it. */
export function withCliAccess(
  state: IntegrationsState,
  on: true,
): IntegrationsState;
export function withCliAccess(
  state: IntegrationsState,
  on: boolean,
): IntegrationsState | undefined;
export function withCliAccess(
  state: IntegrationsState,
  on: boolean,
): IntegrationsState | undefined {
  if (on) {
    if (cliAccessOn(state)) return state;
    return aiAccessOn(state) ? "calm" : "cliOnly";
  }
  if (state === "local" || state === "localAllowed") return undefined;
  return aiAccessOn(state) ? "aiOnly" : "off";
}

export interface Scenario {
  recovery: RecoveryState;
  access: AccessState;
  devices: DevicesState;
  integrations: IntegrationsState;
}

/** From most to least urgent. `queued` is an action waiting behind the one that leads. */
export type Tone =
  "risk" | "action" | "queued" | "now" | "recent" | "pending" | "tip" | "calm";

export type CardKind =
  "access" | "devices" | "recovery" | "apps" | "integrations";

export interface CardStatus {
  kind: CardKind;
  tone: Tone;
  sentence: string;
  /** The step, as a verb, for the dock's button on the page. Without it, the
   *  dock says the sentence. */
  ask?: string;
}

export interface StatusInput {
  scenario: Scenario;
  /** Browsers signed out from the Devices section since the page opened. */
  signedOut: ReadonlySet<string>;
  /** Browsers left in the list, after those signed out. */
  browserCount: number;
  apps: { total: number; notifying: number };
  /** The access method the identity is signed in with right now. */
  signedInWith: string;
}

/** The order the cards are shown in. */
export const CARD_ORDER: CardKind[] = [
  "access",
  "devices",
  "recovery",
  "apps",
  "integrations",
];

/** Which card leads when several ask for action. */
const PRIORITY: CardKind[] = [
  "recovery",
  "access",
  "devices",
  "integrations",
  "apps",
];

type Said = Pick<CardStatus, "tone" | "sentence" | "ask">;

const RECOVERY: Record<RecoveryState, Said> = {
  none: {
    tone: "action",
    sentence: "No recovery set up yet",
    ask: "Set up recovery",
  },
  unverified: {
    tone: "action",
    sentence: "Recovery phrase not verified yet",
    ask: "Verify your recovery phrase",
  },
  usedPhrase: { tone: "now", sentence: "You're signed in with your phrase" },
  emailPending: {
    tone: "pending",
    sentence: "Waiting for your email",
    ask: "Confirm your recovery email",
  },
  phraseReset: {
    tone: "recent",
    sentence: "Your recovery phrase was reset",
    ask: "Review the phrase reset",
  },
  noEmail: { tone: "tip", sentence: "Phrase set up, no email yet" },
  emailOnly: { tone: "tip", sentence: "Email set up, no phrase yet" },
  calm: { tone: "calm", sentence: "Phrase and email set up" },
};

const accessSaid = ({ scenario, signedInWith }: StatusInput): Said =>
  ({
    one: {
      tone: "action",
      sentence: "Only one way to sign in",
      ask: "Add a second way to sign in",
    },
    newPasskey: {
      tone: "recent",
      sentence: "New passkey from another device",
      ask: "Review the new passkey",
    },
    unused: { tone: "tip", sentence: "YubiKey unused for 8 months" },
    calm: { tone: "calm", sentence: `Signed in with ${signedInWith}` },
  })[scenario.access] as Said;

const INTEGRATIONS: Record<IntegrationsState, Said> = {
  local: {
    tone: "risk",
    sentence: "Any local program can connect",
    ask: "Review local access",
  },
  localAllowed: { tone: "tip", sentence: "Any local program can connect" },
  cliConfirm: {
    tone: "pending",
    sentence: "ICP CLI is waiting for approval",
    ask: "Review the ICP CLI request",
  },
  aiOnly: { tone: "calm", sentence: "AI access on" },
  cliOnly: { tone: "calm", sentence: "CLI access on" },
  off: { tone: "tip", sentence: "AI and CLI aren't connected" },
  calm: { tone: "calm", sentence: "AI and CLI access on" },
};

const devicesSaid = ({
  scenario,
  signedOut,
  browserCount,
}: StatusInput): Said => {
  switch (scenario.devices) {
    case "newBrowser":
      return signedOut.has("firefox")
        ? { tone: "recent", sentence: "Firefox was signed out" }
        : {
            tone: "recent",
            sentence: "New sign-in from Firefox",
            ask: "Review the Firefox sign-in",
          };
    case "many":
      return browserCount > 4
        ? {
            tone: "tip",
            sentence: `Signed in on ${browserCount} browsers`,
          }
        : { tone: "calm", sentence: `Signed in on ${browserCount} browsers` };
    case "known":
    case "calm":
      return {
        tone: "calm",
        sentence: `Signed in on ${browserCount} browsers`,
      };
  }
};

const appsSaid = ({ apps }: StatusInput): Said =>
  apps.total === 0
    ? { tone: "calm", sentence: "No apps yet" }
    : apps.notifying === 0
      ? { tone: "calm", sentence: "Change permissions or remove apps" }
      : {
          tone: "calm",
          sentence: `${apps.notifying} of ${apps.total} apps can notify you`,
        };

export const deriveStatus = (input: StatusInput): CardStatus[] => {
  const { scenario, signedOut } = input;
  const said: Record<CardKind, Said> = {
    recovery: RECOVERY[scenario.recovery],
    access: accessSaid(input),
    devices: devicesSaid(input),
    integrations: INTEGRATIONS[scenario.integrations],
    apps: appsSaid(input),
  };

  // 1. One situation, told once, by the card that can resolve it.
  const newBrowserLive =
    scenario.devices === "newBrowser" && !signedOut.has("firefox");
  if (scenario.recovery === "usedPhrase") {
    said.access = {
      tone: "action",
      sentence: "Add a passkey to sign in next time",
      ask: "Add a passkey",
    };
  }
  if (scenario.recovery === "none" && scenario.access === "one") {
    said.access = {
      tone: "action",
      sentence: "Only one way in, no recovery",
      ask: "Add a second way to sign in",
    };
  }
  if (scenario.access === "newPasskey" && newBrowserLive) {
    said.access = {
      tone: "recent",
      sentence: "New passkey from Firefox",
      ask: "Review the new passkey",
    };
  }
  if (scenario.recovery === "phraseReset" && newBrowserLive) {
    said.recovery = {
      tone: "risk",
      sentence: "Phrase reset from a new browser",
      ask: "Review the phrase reset",
    };
    if (scenario.access === "newPasskey") {
      // The incident tells the whole story under that title; the row keeps
      // to its own part of it.
      said.devices = {
        tone: "risk",
        sentence: "New sign-in from Firefox",
        ask: "Check what Firefox changed",
      };
    }
  }

  // 2. During an ongoing situation or a risk, only the cards resolving it stay loud.
  const risks = CARD_ORDER.filter((kind) => said[kind].tone === "risk");
  const focus: CardKind[] | undefined =
    scenario.recovery === "usedPhrase"
      ? ["access", "recovery", ...risks]
      : risks.length > 0
        ? risks
        : undefined;
  if (focus !== undefined) {
    for (const kind of CARD_ORDER) {
      const { tone } = said[kind];
      if (!focus.includes(kind) && (tone === "action" || tone === "pending")) {
        said[kind] = { ...said[kind], tone: "tip" };
      }
    }
  }

  // 3. Only one card asks for action; the others wait behind it.
  const lead = PRIORITY.find((kind) => said[kind].tone === "action");
  for (const kind of PRIORITY) {
    if (kind !== lead && said[kind].tone === "action") {
      said[kind] = { ...said[kind], tone: "queued" };
    }
  }

  return CARD_ORDER.map((kind) => ({ kind, ...said[kind] }));
};

/** Whether anything on the panel waits on the user, for the dot on the mobile button. */
/** A card that is loud: it holds a risk or the one action the user should take. */
export const isLoud = ({ tone }: CardStatus): boolean =>
  tone === "risk" || tone === "action";

/** The card the collapsed panel speaks for: a risk first, then the action. */
export const leadOf = (cards: CardStatus[]): CardStatus | undefined => {
  const byPriority = PRIORITY.map((kind) =>
    cards.find((card) => card.kind === kind),
  ).filter((card) => card !== undefined);
  return (
    byPriority.find(({ tone }) => tone === "risk") ??
    byPriority.find(({ tone }) => tone === "action")
  );
};

/** What the loud card offers. With `resolve`, the step finishes right there, as
 *  the concept fakes it; without, the button opens the card's section. */
export interface LeadAction {
  /** A verb alone: the button stands at the end of its card's row, beside
   *  the card's title and sentence, which say what it acts on. */
  label: string;
  resolve?: { patch: Partial<Scenario>; message: string };
  /** The product's own flow runs first, and the step resolves once it's done. */
  flow?: "verifyPhrase";
}

export const leadActionOf = (
  { kind, tone }: CardStatus,
  scenario: Scenario,
): LeadAction => {
  if (tone === "risk") {
    return { label: "Review" };
  }
  if (kind === "recovery" && scenario.recovery === "unverified") {
    return {
      label: "Verify",
      resolve: {
        patch: { recovery: "calm" },
        message: "Recovery phrase verified",
      },
      flow: "verifyPhrase",
    };
  }
  if (kind === "recovery" && scenario.recovery === "none") {
    return {
      label: "Set up",
      resolve: {
        patch: { recovery: "calm" },
        message: "Recovery phrase set up",
      },
    };
  }
  if (kind === "access") {
    return { label: "Add" };
  }
  return { label: "Review" };
};

export const needsAttention = (cards: CardStatus[]): boolean =>
  cards.some(({ tone }) =>
    (["risk", "action", "queued", "pending"] as Tone[]).includes(tone),
  );

/** One way to answer an incident, and what the answer changes. */
export interface IncidentAnswer {
  label: string;
  patch: Partial<Scenario>;
  message: string;
}

/**
 * A risk told as one story: something changed the identity from elsewhere,
 * or left it open, and only the user can tell whether that was them. Until
 * they answer, it meets them on every visit and asks for the cards it
 * covers, which stop asking on their own.
 */
export interface Incident {
  /** The cards whose risk this tells. */
  kinds: CardKind[];
  title: string;
  /** The one question the answers reply to. */
  question: string;
  /** What happened, oldest first, each by the card it touched. */
  facts: { kind: CardKind; text: string; meta: string }[];
  /** The step, as a verb, where the incident is only mentioned. */
  ask: string;
  /** It wasn't the user: undo what was done. */
  secure: IncidentAnswer & {
    /** What the answer will do, said before it is given. */
    detail: string;
  };
  /** It was the user: the change stays and goes quiet. */
  confirm: IncidentAnswer;
}

export const incidentOf = ({
  scenario,
  signedOut,
}: Pick<StatusInput, "scenario" | "signedOut">): Incident | undefined => {
  const newBrowserLive =
    scenario.devices === "newBrowser" && !signedOut.has("firefox");
  if (scenario.recovery === "phraseReset" && newBrowserLive) {
    const passkey = scenario.access === "newPasskey";
    const access: Partial<Scenario> = passkey ? { access: "calm" } : {};
    return {
      kinds: passkey
        ? ["devices", "access", "recovery"]
        : ["devices", "recovery"],
      title: passkey
        ? "Firefox changed how you sign in"
        : "Firefox reset your recovery phrase",
      question: "Was this you?",
      facts: [
        {
          kind: "devices",
          text: "Signed in from Firefox",
          meta: "12 min ago",
        },
        ...(passkey
          ? [
              {
                kind: "access" as const,
                text: "Added a passkey",
                meta: "10 min ago",
              },
            ]
          : []),
        {
          kind: "recovery",
          text: "Reset the recovery phrase",
          meta: "9 min ago",
        },
      ],
      ask: "Check what Firefox changed",
      secure: {
        label: "This wasn't me",
        detail: passkey
          ? "Signs Firefox out and removes its passkey. Then you set a new recovery phrase."
          : "Signs Firefox out. Then you set a new recovery phrase.",
        // The phrase Firefox set is void, so recovery starts over.
        patch: { devices: "calm", recovery: "none", ...access },
        message: "Firefox signed out",
      },
      confirm: {
        label: "This was me",
        patch: { devices: "known", recovery: "calm", ...access },
        message: "Firefox stays signed in",
      },
    };
  }
  if (scenario.integrations === "local") {
    return {
      kinds: ["integrations"],
      title: "Any local program can connect",
      question: "Did you set this up?",
      facts: [
        {
          kind: "integrations",
          text: "Local server on this computer",
          meta: "No approval to connect",
        },
      ],
      ask: "Review local access",
      secure: {
        label: "Remove it",
        detail: "Removes the server. AI and CLI tools ask before they connect.",
        patch: { integrations: "calm" },
        message: "Local server removed",
      },
      confirm: {
        label: "Keep it",
        patch: { integrations: "localAllowed" },
        message: "Local server kept",
      },
    };
  }
  return undefined;
};
