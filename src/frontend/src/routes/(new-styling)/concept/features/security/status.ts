/**
 * What each card in the identity panel says, and how loudly.
 *
 * A card says one thing at a time, in one of four styles (Thomas's): neutral
 * for where it stands, info for something to know or a step it suggests,
 * warning for a step it urges, risk for a change only the user can tell was
 * theirs.
 *
 * Three rules, carried over from the team's prototype:
 * 1. One situation is told once, by the card that can resolve it.
 * 2. During an ongoing situation or a real risk, only the cards that resolve it stay
 *    loud; every other warning drops to info.
 * 3. Only one card urges a step; further warnings wait as info behind it.
 */

/** Thomas's list. `calm`: both the phrase and the email. One of the two is
 *  enough, so `phraseOnly` and `emailOnly` are calm too and never ask for
 *  the other. `usedPhrase`, `usedEmail`: signed in through recovery, one
 *  state to the user whichever way it was (Thomas). `phraseReset`: the
 *  phrase was changed from another browser.
 *
 *  Two that wait on the user (Thomas, 19 Nov): `unverified`, a phrase
 *  written down but never checked, so it may have been written down wrong,
 *  and `emailPending`, an address whose code hasn't come back. Each has a
 *  `…WithEmail` / `…WithPhrase` twin where the other way already works. */
export type RecoveryState =
  | "none"
  | "usedPhrase"
  | "usedEmail"
  | "phraseReset"
  | "phraseOnly"
  | "emailOnly"
  | "unverified"
  | "unverifiedWithEmail"
  | "emailPending"
  | "emailPendingWithPhrase"
  | "calm";
/** Where each way back in stands, as its row says it. `unchecked`: a phrase
 *  written down and never checked. `pending`: an email waiting for its code. */
export type MethodStatus =
  "off" | "unchecked" | "pending" | "on" | "inUse" | "new";

export const phraseStatus = (state: RecoveryState): MethodStatus => {
  switch (state) {
    case "none":
    case "emailOnly":
    case "emailPending":
      return "off";
    case "unverified":
    case "unverifiedWithEmail":
      return "unchecked";
    case "usedPhrase":
      return "inUse";
    case "phraseReset":
      return "new";
    default:
      return "on";
  }
};

export const emailStatus = (state: RecoveryState): MethodStatus => {
  switch (state) {
    case "none":
    case "phraseOnly":
    case "unverified":
      return "off";
    case "emailPending":
    case "emailPendingWithPhrase":
      return "pending";
    case "usedEmail":
      return "inUse";
    default:
      return "on";
  }
};

/** A way that gets the user back in today. An unchecked phrase and a pending
 *  email don't count yet: either may not work. */
/** Signed in through recovery, by the phrase or by the email. */
export const throughRecovery = (state: RecoveryState): boolean =>
  state === "usedPhrase" || state === "usedEmail";

export const isOn = (status: MethodStatus) =>
  status === "on" || status === "inUse" || status === "new";

/** The state both ways add up to, once one of them changes. A pair the
 *  concept has no state for keeps the step still to take: an unchecked
 *  phrase over a pending email. */
export const recoveryOf = (
  phrase: MethodStatus,
  email: MethodStatus,
): RecoveryState => {
  if (phrase === "unchecked") {
    return isOn(email) ? "unverifiedWithEmail" : "unverified";
  }
  if (email === "pending") {
    return isOn(phrase) ? "emailPendingWithPhrase" : "emailPending";
  }
  if (isOn(phrase)) {
    return isOn(email) ? "calm" : "phraseOnly";
  }
  return isOn(email) ? "emailOnly" : "none";
};

/** `one`: a single passkey that syncs (a password manager's). `oneLocal`: a
 *  single passkey kept on this device alone (Windows Hello, a security key),
 *  so losing the device loses the identity unless recovery is set up.
 *  `unused`: one of the ways in hasn't been used for months. */
export type AccessState = "one" | "oneLocal" | "unused" | "calm";

/** How many ways in the Access section lists. */
export const accessCount = (state: AccessState): number =>
  state === "one" || state === "oneLocal" ? 1 : 3;

/** The ways in the Access section lists, by id: the one this browser
 *  signed in with first. */
export const accessIds = (state: AccessState): string[] =>
  accessCount(state) === 1 ? ["current"] : ["current", "yubikey", "google"];

/** The ways in left once those removed from the Access section are gone. */
export const waysLeft = (
  state: AccessState,
  removed: ReadonlySet<string> = new Set(),
): string[] => accessIds(state).filter((id) => !removed.has(id));
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

/** The four styles a card speaks in, loudest first. */
export type Tone = "risk" | "warning" | "info" | "neutral";

export type CardKind =
  "access" | "devices" | "recovery" | "apps" | "integrations";

export interface CardStatus {
  kind: CardKind;
  tone: Tone;
  sentence: string;
  /** The step, as a verb, for the dock's button on the page. Without it, the
   *  dock says the sentence. */
  ask?: string;
  /** Something that happened rather than a step: once seen, it's done. */
  news?: boolean;
}

export interface StatusInput {
  scenario: Scenario;
  /** Browsers signed out from the Devices section since the page opened. */
  signedOut: ReadonlySet<string>;
  /** Browsers left in the list, after those signed out. */
  browserCount: number;
  /** Ways in removed from the Access section since the page opened. */
  removedAccess?: ReadonlySet<string>;
  apps: { total: number; notifying: number };
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

type Said = Pick<CardStatus, "tone" | "sentence" | "ask" | "news">;

const recoverySaid = ({ scenario }: StatusInput): Said => {
  switch (scenario.recovery) {
    case "none":
      // Nothing here explains what recovery is; the section does. A single
      // passkey on this device alone turns the suggestion into a warning.
      return scenario.access === "oneLocal"
        ? {
            tone: "warning",
            sentence: "Only this device can sign you in",
            ask: "Set up recovery",
          }
        : { tone: "info", sentence: "Not set up yet", ask: "Set up recovery" };
    case "usedPhrase":
    case "usedEmail":
      // Which way it was matters little (Thomas): the glyph beside it says.
      return { tone: "info", sentence: "Signed in through recovery" };
    case "phraseReset":
      return {
        tone: "info",
        sentence: "Recovery phrase was changed",
        ask: "Review the phrase change",
        news: true,
      };
    case "unverified":
    case "unverifiedWithEmail":
      // A phrase written down wrong fails only when it's needed. Loud while
      // it's the one way back; with a working email it's a step to take,
      // not a risk, and one way is enough (Thomas).
      return {
        tone: scenario.recovery === "unverified" ? "warning" : "info",
        sentence: "Recovery phrase not verified yet",
        ask: "Verify your recovery phrase",
      };
    case "emailPending":
    case "emailPendingWithPhrase":
      return {
        tone: "info",
        sentence: "Waiting for your email",
        ask: "Confirm your recovery email",
      };
    case "phraseOnly":
      return { tone: "neutral", sentence: "Recovery phrase set up" };
    case "emailOnly":
      return { tone: "neutral", sentence: "Recovery email set up" };
    case "calm":
      return { tone: "neutral", sentence: "Phrase and email set up" };
  }
};

const waysIn = (count: number) =>
  count === 1 ? "1 way to sign in" : `${count} ways to sign in`;

const accessSaid = ({ scenario, removedAccess }: StatusInput): Said =>
  scenario.access === "unused" && removedAccess?.has("yubikey") !== true
    ? { tone: "info", sentence: "YubiKey unused for 8 months" }
    : {
        tone: "neutral",
        sentence: waysIn(waysLeft(scenario.access, removedAccess).length),
      };

const INTEGRATIONS: Record<IntegrationsState, Said> = {
  local: {
    tone: "risk",
    sentence: "Any local program can connect",
    ask: "Review local access",
  },
  localAllowed: { tone: "neutral", sentence: "Any local program can connect" },
  cliConfirm: {
    tone: "info",
    sentence: "ICP CLI is waiting for approval",
    ask: "Review the ICP CLI request",
  },
  aiOnly: { tone: "neutral", sentence: "AI access on" },
  cliOnly: { tone: "neutral", sentence: "CLI access on" },
  off: { tone: "neutral", sentence: "AI and CLI aren't connected" },
  calm: { tone: "neutral", sentence: "AI and CLI access on" },
};

const devicesSaid = ({
  scenario,
  signedOut,
  browserCount,
}: StatusInput): Said =>
  scenario.devices === "newBrowser" && !signedOut.has("firefox")
    ? {
        tone: "info",
        sentence: "New sign-in from Firefox",
        ask: "Review the Firefox sign-in",
        news: true,
      }
    : {
        tone: "neutral",
        sentence:
          browserCount === 1
            ? "Signed in on 1 browser"
            : `Signed in on ${browserCount} browsers`,
      };

const appsSaid = ({ apps }: StatusInput): Said =>
  apps.total === 0
    ? { tone: "neutral", sentence: "No apps yet" }
    : apps.notifying === 0
      ? { tone: "neutral", sentence: "Change notifications or forget apps" }
      : {
          tone: "neutral",
          sentence: `${apps.notifying} of ${apps.total} apps can notify you`,
        };

export const deriveStatus = (input: StatusInput): CardStatus[] => {
  const { scenario, signedOut } = input;
  const said: Record<CardKind, Said> = {
    recovery: recoverySaid(input),
    access: accessSaid(input),
    devices: devicesSaid(input),
    integrations: INTEGRATIONS[scenario.integrations],
    apps: appsSaid(input),
  };

  // 1. One situation, told once, by the card that can resolve it.
  const newBrowserLive =
    scenario.devices === "newBrowser" && !signedOut.has("firefox");
  if (throughRecovery(scenario.recovery)) {
    // In through recovery, the passkey is most likely gone: Access urges a
    // new way in, so next time doesn't take recovery again.
    said.access = {
      tone: "warning",
      sentence: "Add a way to sign in",
      ask: "Add a passkey",
    };
  }
  if (scenario.recovery === "phraseReset" && newBrowserLive) {
    // Recovery changed by a browser that has only just signed in: together
    // the strongest warning there is, on both cards.
    const ask = "Check what Firefox changed";
    said.recovery = {
      tone: "risk",
      sentence: "Recovery phrase changed from Firefox",
      ask,
    };
    said.devices = { tone: "risk", sentence: "New sign-in from Firefox", ask };
  }

  // 2. During an ongoing situation or a risk, only the cards resolving it stay loud.
  const risks = CARD_ORDER.filter((kind) => said[kind].tone === "risk");
  const focus: CardKind[] | undefined = throughRecovery(scenario.recovery)
    ? ["access", "recovery", ...risks]
    : risks.length > 0
      ? risks
      : undefined;
  if (focus !== undefined) {
    for (const kind of CARD_ORDER) {
      if (!focus.includes(kind) && said[kind].tone === "warning") {
        said[kind] = { ...said[kind], tone: "info" };
      }
    }
  }

  // 3. Only one card urges a step; the others wait behind it, as info.
  const lead = PRIORITY.find((kind) => said[kind].tone === "warning");
  for (const kind of PRIORITY) {
    if (kind !== lead && said[kind].tone === "warning") {
      said[kind] = { ...said[kind], tone: "info" };
    }
  }

  return CARD_ORDER.map((kind) => ({ kind, ...said[kind] }));
};

/** A card that is loud: it holds a risk or the one step the user should take. */
export const isLoud = ({ tone }: CardStatus): boolean =>
  tone === "risk" || tone === "warning";

/** The card the collapsed panel speaks for: a risk first, then the warning. */
export const leadOf = (cards: CardStatus[]): CardStatus | undefined => {
  const byPriority = PRIORITY.map((kind) =>
    cards.find((card) => card.kind === kind),
  ).filter((card) => card !== undefined);
  return (
    byPriority.find(({ tone }) => tone === "risk") ??
    byPriority.find(({ tone }) => tone === "warning")
  );
};

/** What the loud card offers. With `resolve`, the step finishes right there, as
 *  the concept fakes it; without, the button opens the card's section. */
export interface LeadAction {
  /** A verb alone: the button stands at the end of its card's row, beside
   *  the card's title and sentence, which say what it acts on. */
  label: string;
  resolve?: { patch: Partial<Scenario>; message: string };
}

export const leadActionOf = (
  { kind, tone }: CardStatus,
  scenario: Scenario,
): LeadAction => {
  if (tone === "risk") {
    return { label: "Review" };
  }
  if (kind === "recovery" && phraseStatus(scenario.recovery) === "unchecked") {
    return { label: "Verify" };
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

/** Whether anything waits on the user, for the dot on the account: a risk,
 *  a warning, or a step suggested. News asks for nothing. */
export const needsAttention = (cards: CardStatus[]): boolean =>
  cards.some(
    ({ tone, ask, news }) =>
      tone === "risk" ||
      tone === "warning" ||
      (tone === "info" && ask !== undefined && news !== true),
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
    return {
      kinds: ["devices", "recovery"],
      title: "Firefox changed your recovery phrase",
      question: "Was this you?",
      facts: [
        {
          kind: "devices",
          text: "Signed in from Firefox",
          meta: "12 min ago",
        },
        {
          kind: "recovery",
          text: "Changed the recovery phrase",
          meta: "9 min ago",
        },
      ],
      ask: "Check what Firefox changed",
      secure: {
        label: "This wasn't me",
        detail: "Signs Firefox out. Then you set a new recovery phrase.",
        // The phrase Firefox set is void, so recovery starts over.
        patch: { devices: "calm", recovery: "none" },
        message: "Firefox signed out",
      },
      confirm: {
        label: "This was me",
        patch: { devices: "known", recovery: "calm" },
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
