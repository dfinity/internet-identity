// The whole frontend: a sign-in button that also asks Internet Identity
// whether this app may notify and what the user is called, the room read every
// few seconds and drawn as a chat, a box to type in, a menu to claim a name in,
// the people in the room beside it, and a panel with the notification
// library's delivery stats.

import {
  AuthClient,
  NotificationsUnsupportedError,
} from "@icp-sdk/auth/client";
import { Actor, HttpAgent, type Identity } from "@icp-sdk/core/agent";
import { IDL } from "@icp-sdk/core/candid";
import { Principal } from "@icp-sdk/core/principal";

// ===== Configuration =====

const II_URL = import.meta.env.VITE_II_URL ?? "https://id.ai/authorize";
const II_CANISTER_ID =
  import.meta.env.VITE_II_CANISTER_ID ?? "rdmx6-jaaaa-aaaaa-aaadq-cai";

/** How often the room is read: nothing tells the page somebody else wrote. */
const POLL_MS = 3_000;

/**
 * This app's canister. Served from the canister, the page says so (the
 * canister writes its id into the page it serves) and so does the host
 * (`<id>.icp0.io`, `<id>.localhost`); the dev server is told instead, with
 * `?canisterId=` or `VITE_CANISTER_ID`.
 */
const canisterId = (): Principal => {
  const candidates = [
    new URLSearchParams(location.search).get("canisterId"),
    import.meta.env.VITE_CANISTER_ID,
    document.querySelector<HTMLMetaElement>('meta[name="canister-id"]')
      ?.content,
    location.hostname.split(".")[0],
  ];
  for (const candidate of candidates) {
    if (candidate === null || candidate === undefined) continue;
    try {
      return Principal.fromText(candidate);
    } catch {
      // not a principal, try the next
    }
  }
  throw new Error(
    "Which canister? Open the page from it, or pass ?canisterId=<id>.",
  );
};

const isLocalHost = (): boolean =>
  location.hostname === "localhost" ||
  location.hostname === "127.0.0.1" ||
  location.hostname.endsWith(".localhost");

/** Off mainnet the root key has to be fetched, or every certificate fails. */
const fetchRootKey =
  import.meta.env.VITE_FETCH_ROOT_KEY === undefined
    ? isLocalHost()
    : import.meta.env.VITE_FETCH_ROOT_KEY === "1";

// ===== The canister's interface =====

interface Member {
  who: Principal;
  joined_at: bigint;
}

interface Message {
  from: Principal;
  at: bigint;
  text: string;
}

interface Room {
  members: Member[];
  messages: Message[];
  /** The claimed names of the members and of whoever wrote a message. */
  names: [Principal, string][];
}

interface Counts {
  queued: bigint;
  accepted: bigint;
  received: bigint;
  opened: bigint;
  dropped: bigint;
  deferred: bigint;
}

interface Metrics {
  hours: { start: bigint; counts: Counts }[];
  funnels: { name: string; counts: Counts }[];
  backlog: bigint;
  misconfigured: [] | [{ why: Record<string, null>; since: bigint }];
}

/** What the canister's notification library is configured with. */
interface NotificationConfig {
  origin: [] | [string];
  sender: [] | [Principal];
}

interface ChatService {
  join: () => Promise<void>;
  leave: () => Promise<void>;
  send: (text: string) => Promise<void>;
  /** Returns the name given, which is the one asked for unless taken. */
  claim_name: (wanted: string) => Promise<string>;
  room: () => Promise<Room>;
  notification_metrics: () => Promise<Metrics>;
  notification_config: () => Promise<NotificationConfig>;
}

const idlFactory: IDL.InterfaceFactory = ({ IDL }) => {
  const Counts = IDL.Record({
    queued: IDL.Nat64,
    accepted: IDL.Nat64,
    received: IDL.Nat64,
    opened: IDL.Nat64,
    dropped: IDL.Nat64,
    deferred: IDL.Nat64,
  });
  const Room = IDL.Record({
    members: IDL.Vec(IDL.Record({ who: IDL.Principal, joined_at: IDL.Nat64 })),
    messages: IDL.Vec(
      IDL.Record({ from: IDL.Principal, at: IDL.Nat64, text: IDL.Text }),
    ),
    names: IDL.Vec(IDL.Tuple(IDL.Principal, IDL.Text)),
  });
  const Metrics = IDL.Record({
    hours: IDL.Vec(IDL.Record({ start: IDL.Nat64, counts: Counts })),
    funnels: IDL.Vec(IDL.Record({ name: IDL.Text, counts: Counts })),
    backlog: IDL.Nat64,
    misconfigured: IDL.Opt(
      IDL.Record({
        why: IDL.Variant({
          Origin: IDL.Null,
          Sender: IDL.Null,
          NotAuthorizedSender: IDL.Null,
        }),
        since: IDL.Nat64,
      }),
    ),
  });
  return IDL.Service({
    join: IDL.Func([], [], []),
    leave: IDL.Func([], [], []),
    send: IDL.Func([IDL.Text], [], []),
    claim_name: IDL.Func([IDL.Text], [IDL.Text], []),
    room: IDL.Func([], [Room], ["query"]),
    notification_metrics: IDL.Func([], [Metrics], ["query"]),
    notification_config: IDL.Func(
      [],
      [
        IDL.Record({
          origin: IDL.Opt(IDL.Text),
          sender: IDL.Opt(IDL.Principal),
        }),
      ],
      ["query"],
    ),
  });
};

const actorFor = async (identity?: Identity): Promise<ChatService> => {
  const agent = await HttpAgent.create({
    host: location.origin,
    identity,
    shouldFetchRootKey: fetchRootKey,
  });
  return Actor.createActor<ChatService>(idlFactory, {
    agent,
    canisterId: canisterId(),
  });
};

// ===== The name Internet Identity may hand over =====

type Icrc3Value =
  | { Nat: bigint }
  | { Int: bigint }
  | { Blob: Uint8Array }
  | { Text: string }
  | { Array: Icrc3Value[] }
  | { Map: [string, Icrc3Value][] };

/** What the signed attributes are encoded as. */
const Icrc3ValueIdl = IDL.Rec();
Icrc3ValueIdl.fill(
  IDL.Variant({
    Nat: IDL.Nat,
    Int: IDL.Int,
    Blob: IDL.Vec(IDL.Nat8),
    Text: IDL.Text,
    Array: IDL.Vec(Icrc3ValueIdl),
    Map: IDL.Vec(IDL.Tuple(IDL.Text, Icrc3ValueIdl)),
  }),
);

/**
 * The name in a signed attribute bundle, or none. The bundle is a map: the
 * name sits under `name`, or under a scoped key such as
 * `openid:<issuer>:name`, beside `implicit:*` entries about the bundle itself.
 * Its signature is not checked: a name is what somebody wants to be called,
 * and the room treats one Internet Identity handed over no differently from
 * one typed in.
 */
const nameAmong = (data: Uint8Array): string | undefined => {
  const [bundle] = IDL.decode([Icrc3ValueIdl], data) as unknown as [Icrc3Value];
  if (!("Map" in bundle)) return undefined;
  for (const [key, value] of bundle.Map) {
    const isName =
      key === "name" || (key.endsWith(":name") && !key.startsWith("implicit:"));
    if (isName && "Text" in value && value.Text.trim() !== "") {
      return value.Text.trim();
    }
  }
  return undefined;
};

// ===== The page =====

const element = <T extends HTMLElement>(id: string): T => {
  const found = document.getElementById(id);
  if (found === null) throw new Error(`no #${id}`);
  return found as T;
};

const appEl = element<HTMLDivElement>("app");
const chatEl = element<HTMLElement>("chat");
const authBtn = element<HTMLButtonElement>("auth");
const accountBtn = element<HTMLButtonElement>("account");
const meAvatarEl = element<HTMLSpanElement>("me-avatar");
const meEl = element<HTMLSpanElement>("me");
const menuEl = element<HTMLDivElement>("account-menu");
const menuAvatarEl = element<HTMLSpanElement>("menu-avatar");
const menuNameEl = element<HTMLParagraphElement>("menu-name");
const menuPrincipalEl = element<HTMLParagraphElement>("menu-principal");
const claimEl = element<HTMLFormElement>("claim");
const nameEl = element<HTMLInputElement>("name");
const claimBtn = element<HTMLButtonElement>("claim-btn");
const peopleSummaryEl = element<HTMLSpanElement>("people-summary");
const peopleOpenBtn = element<HTMLButtonElement>("people-open");
const scrollerEl = element<HTMLDivElement>("scroller");
const messagesEl = element<HTMLDivElement>("messages");
const jumpBtn = element<HTMLButtonElement>("jump");
const noticeEl = element<HTMLDivElement>("notice");
const statusEl = element<HTMLParagraphElement>("status");
const statusDismissBtn = element<HTMLButtonElement>("status-dismiss");
const composeEl = element<HTMLFormElement>("compose");
const textEl = element<HTMLTextAreaElement>("text");
const counterEl = element<HTMLSpanElement>("counter");
const sendBtn = element<HTMLButtonElement>("send");
const peopleEl = element<HTMLElement>("people");
const peopleCountEl = element<HTMLSpanElement>("people-count");
const peopleCloseBtn = element<HTMLButtonElement>("people-close");
const scrimEl = element<HTMLDivElement>("scrim");
const membersEl = element<HTMLUListElement>("members");
const statsToggle = element<HTMLButtonElement>("stats-toggle");
const statsEl = element<HTMLDivElement>("stats");
const announcerEl = element<HTMLParagraphElement>("announcer");

const client = new AuthClient({
  identityProvider: { authorizeUrl: II_URL, canisterId: II_CANISTER_ID },
  agentOptions: { host: location.origin, shouldFetchRootKey: fetchRootKey },
});

/** The longest message the canister keeps, in characters. */
const MAX_TEXT = 500;
/** From this length on, the composer counts characters. */
const COUNT_FROM = 400;
/** Messages from one sender this close together share a group. */
const GROUP_GAP_NS = 5n * 60n * 1_000_000_000n;
/** How near the end of the list still counts as reading the newest. */
const NEAR_END_PX = 96;

/** Where the people list stops being a drawer and sits beside the chat. */
const wide = window.matchMedia("(min-width: 48rem)");
const finePointer = window.matchMedia("(pointer: fine)");
const reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");

/** Whether this identity is in the room, as the last read of it said. */
let joined = false;
let busy = false;
/** What the sign-in button says while `busy`. */
let busyLabel = "…";
/** A claim in flight. */
let claiming = false;
/** A join in flight, so a room that says we are out is not joined twice. */
let rejoining = false;
/** Who is called what, by principal, as the last read of the room said. */
let names = new Map<string, string>();
/** The room as last read: none until the first read answers. */
let room: Room | undefined;
/** The room as it was last drawn, so an unchanged room is left alone. */
let shownRoom = "";
/** Whether the messages have been drawn, rather than the skeleton. */
let loaded = false;
/** The messages on screen, by key, so the ones that arrive since stand out. */
let shownKeys = new Set<string>();
/** Whether the list is scrolled to its end, where new messages keep it. */
let pinned = true;

/**
 * A message this page sent, shown until the room has it. One is sent at a
 * time; one that failed stays, to be sent again or deleted.
 */
interface Outgoing {
  id: number;
  text: string;
  state: "sending" | "sent" | "failed";
  /** When it was written, by this computer's clock. */
  at: bigint;
  /** The newest message in the room when it went out: its copy is not older. */
  after: bigint | undefined;
}

let outgoing: Outgoing[] = [];
let outgoingIds = 0;

const sending = (): boolean =>
  outgoing.some((item) => item.state === "sending");

const me = (): Principal | undefined => client.getPrincipal();

/** A principal as it is shown: its first and last groups. */
const short = (principal: Principal): string => {
  const groups = principal.toText().split("-");
  return groups.length > 1 ? `${groups[0]}…${groups.at(-1)}` : groups[0]!;
};

/** Somebody as they are shown: their claimed name, or their principal's ends. */
const nameOf = (principal: Principal): string =>
  names.get(principal.toText()) ?? short(principal);

const dateOf = (nanos: bigint): Date => new Date(Number(nanos / 1_000_000n));

const timeOf = (nanos: bigint): string => dateOf(nanos).toLocaleTimeString();

/** A time of day as messages show it: hours and minutes. */
const clockOf = (nanos: bigint): string =>
  dateOf(nanos).toLocaleTimeString([], { hour: "numeric", minute: "2-digit" });

const dayKey = (date: Date): string =>
  `${date.getFullYear()}-${date.getMonth()}-${date.getDate()}`;

/** "Today", "Yesterday", a weekday within the week, or else the date. */
const dayLabel = (date: Date): string => {
  const today = new Date();
  const midnight = (day: Date) =>
    new Date(day.getFullYear(), day.getMonth(), day.getDate()).getTime();
  const daysAgo = Math.round((midnight(today) - midnight(date)) / 86_400_000);
  if (daysAgo === 0) return "Today";
  if (daysAgo === 1) return "Yesterday";
  if (daysAgo > 1 && daysAgo < 7) {
    return date.toLocaleDateString([], { weekday: "long" });
  }
  return date.toLocaleDateString([], {
    weekday: "short",
    day: "numeric",
    month: "long",
    year: date.getFullYear() === today.getFullYear() ? undefined : "numeric",
  });
};

const joinedLabel = (nanos: bigint): string => {
  const day = dayLabel(dateOf(nanos));
  if (day === "Today") return `Joined at ${clockOf(nanos)}`;
  return `Joined ${day === "Yesterday" ? "yesterday" : day}`;
};

const timeElement = (nanos: bigint): HTMLTimeElement => {
  const date = dateOf(nanos);
  const time = document.createElement("time");
  time.dateTime = date.toISOString();
  time.title = date.toLocaleString([], {
    dateStyle: "full",
    timeStyle: "medium",
  });
  time.textContent = clockOf(nanos);
  return time;
};

const describe = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/** Read out by screen readers, which do not watch the list itself. */
const announce = (text: string) => {
  announcerEl.textContent = text;
};

const showStatus = (text: string) => {
  if (text !== "" && text !== statusEl.textContent) announce(text);
  statusEl.textContent = text;
  noticeEl.hidden = text === "";
};

// ===== Avatars and icons =====

/** A hue from the principal, so somebody keeps their colour everywhere. */
const hueOf = (principal: Principal): number => {
  let hash = 2166136261;
  for (const char of principal.toText()) {
    hash = Math.imul(hash ^ char.charCodeAt(0), 16777619);
  }
  return (hash >>> 0) % 360;
};

/** Up to two initials of somebody's name, or the start of their principal. */
const initialsOf = (principal: Principal): string => {
  const name = names.get(principal.toText());
  if (name === undefined) return principal.toText().slice(0, 2).toUpperCase();
  const words = name
    .replace(/_\d+$/, "")
    .split(/\s+/)
    .filter((word) => word !== "");
  const initial = (word: string | undefined): string =>
    word === undefined ? "" : (Array.from(word)[0] ?? "");
  const initials =
    initial(words[0]) + (words.length > 1 ? initial(words.at(-1)) : "");
  return initials === "" ? "?" : initials.toLocaleUpperCase();
};

const paintAvatar = (avatar: HTMLElement, principal: Principal) => {
  avatar.style.setProperty("--hue", String(hueOf(principal)));
  avatar.textContent = initialsOf(principal);
};

/** An avatar; a small one, as in the header's stack, has room for one letter. */
const avatarOf = (principal: Principal, small = false): HTMLSpanElement => {
  const avatar = document.createElement("span");
  avatar.className = "avatar";
  avatar.setAttribute("aria-hidden", "true");
  paintAvatar(avatar, principal);
  if (small) avatar.textContent = Array.from(avatar.textContent ?? "")[0] ?? "";
  return avatar;
};

const ICONS = {
  copy: '<rect x="8" y="8" width="12" height="12" rx="2"/><path d="M16 8V6a2 2 0 0 0-2-2H6a2 2 0 0 0-2 2v8a2 2 0 0 0 2 2h2"/>',
  check: '<path d="M5 12.5l4.5 4.5L19 7.5"/>',
  alert: '<circle cx="12" cy="12" r="9"/><path d="M12 7.5v5.5M12 16.5v.01"/>',
  chat: '<path d="M5 4h14a2 2 0 0 1 2 2v9a2 2 0 0 1-2 2h-7l-5 4v-4H5a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2z"/>',
  chevron: '<path d="m9 6 6 6-6 6"/>',
} as const;

const icon = (name: keyof typeof ICONS): SVGSVGElement => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("class", "icon");
  svg.setAttribute("viewBox", "0 0 24 24");
  svg.setAttribute("aria-hidden", "true");
  svg.innerHTML = ICONS[name];
  return svg;
};

const copyButton = (text: string, label: string): HTMLButtonElement => {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "icon-btn";
  button.title = label;
  button.setAttribute("aria-label", label);
  button.append(icon("copy"));
  button.addEventListener("click", () => {
    Promise.resolve()
      .then(() => navigator.clipboard.writeText(text))
      .then(
        () => {
          button.replaceChildren(icon("check"));
          button.setAttribute("aria-label", "Copied");
          announce("Copied");
          window.setTimeout(() => {
            button.replaceChildren(icon("copy"));
            button.setAttribute("aria-label", label);
          }, 1500);
        },
        () => showStatus("The browser did not allow copying."),
      );
  });
  return button;
};

/** A centred note in the message list: empty, or unreachable. */
const stateBox = (
  symbol: keyof typeof ICONS,
  title: string,
  text: string,
): HTMLElement => {
  const box = document.createElement("div");
  box.className = "state";
  const badge = document.createElement("div");
  badge.className = "state-icon";
  badge.append(icon(symbol));
  const heading = document.createElement("h2");
  heading.textContent = title;
  const body = document.createElement("p");
  body.textContent = text;
  box.append(badge, heading, body);
  return box;
};

// ===== The header, the account menu and the composer =====

const menuOpen = (): boolean => {
  try {
    return menuEl.matches(":popover-open");
  } catch {
    return false;
  }
};

const closeMenu = () => {
  if (menuOpen()) menuEl.hidePopover();
};

const render = () => {
  const signedIn = client.isAuthenticated();
  const principal = me();
  authBtn.textContent = busy ? busyLabel : signedIn ? "Sign out" : "Sign in";
  authBtn.disabled = busy;
  authBtn.classList.toggle("primary", !signedIn);
  accountBtn.hidden = !signedIn || principal === undefined;
  if (principal !== undefined) {
    const name = nameOf(principal);
    meEl.textContent = name;
    accountBtn.title = principal.toText();
    accountBtn.setAttribute("aria-label", `Your name: ${name}`);
    paintAvatar(meAvatarEl, principal);
    paintAvatar(menuAvatarEl, principal);
    menuNameEl.textContent = name;
    menuPrincipalEl.textContent = principal.toText();
  }
  if (!signedIn) closeMenu();
  claimEl.hidden = !signedIn;
  nameEl.placeholder =
    principal !== undefined && names.has(principal.toText())
      ? "Change your name"
      : "Claim a name";
  claimBtn.textContent = claiming ? "Claiming…" : "Claim";
  claimBtn.disabled = busy || claiming || nameEl.value.trim() === "";
  textEl.disabled = !(signedIn && joined);
  textEl.placeholder = signedIn
    ? joined
      ? "Message the room"
      : "Joining…"
    : "Sign in to chat";
  sendBtn.disabled = textEl.disabled || sending() || textEl.value.trim() === "";
  sendBtn.setAttribute("aria-label", sending() ? "Sending…" : "Send");
  const length = textEl.value.length;
  counterEl.hidden = length < COUNT_FROM;
  counterEl.textContent = `${length}/${MAX_TEXT}`;
  counterEl.classList.toggle("full", length >= MAX_TEXT);
};

/** The composer grows with its text, up to five lines, then scrolls. */
const fitText = () => {
  // Measuring collapses the box for a moment, which lets the list grow and
  // pulls a list scrolled to its end off it; a reader at the end stays there.
  textEl.style.height = "auto";
  textEl.style.height = `${textEl.scrollHeight}px`;
  if (pinned) scrollerEl.scrollTop = scrollerEl.scrollHeight;
};

/**
 * Joins again where the room no longer lists this identity: an upgrade empties
 * the room, and a page left open would otherwise say "Joining…" until it is
 * reloaded. Not while the sign-out is leaving, which is what `busy` covers.
 */
const rejoinIfDropped = () => {
  if (joined || rejoining || busy || !client.isAuthenticated()) return;
  rejoining = true;
  void write()
    .then((chat) => chat.join())
    .catch((error) => showStatus(describe(error)))
    .finally(() => {
      rejoining = false;
    });
};

// ===== People =====

const peopleLabel = (count: number): string =>
  count === 0
    ? "Nobody here yet"
    : count === 1
      ? "1 person"
      : `${count} people`;

const memberRow = (
  member: Member,
  own: boolean,
  open: boolean,
): HTMLLIElement => {
  const who = member.who.toText();
  const item = document.createElement("li");
  const details = document.createElement("details");
  details.className = "member";
  details.dataset.who = who;
  details.open = open;
  const summary = document.createElement("summary");
  summary.title = who;
  const text = document.createElement("span");
  text.className = "member-text";
  const name = document.createElement("span");
  name.className = "member-name";
  name.textContent = nameOf(member.who);
  const joinedAt = document.createElement("span");
  joinedAt.className = "member-sub";
  joinedAt.textContent = joinedLabel(member.joined_at);
  text.append(name, joinedAt);
  summary.append(avatarOf(member.who), text);
  if (own) {
    const you = document.createElement("span");
    you.className = "you";
    you.textContent = "You";
    summary.append(you);
  }
  const detail = document.createElement("div");
  detail.className = "member-detail";
  const principal = document.createElement("code");
  principal.textContent = who;
  detail.append(principal, copyButton(who, "Copy principal"));
  details.append(summary, detail);
  item.append(details);
  return item;
};

const drawMembers = (current: Room) => {
  const mine = me()?.toText();
  // You first, then everybody else in the order they joined.
  const members = [...current.members].sort(
    (a, b) => Number(b.who.toText() === mine) - Number(a.who.toText() === mine),
  );
  const open = new Set(
    Array.from(
      membersEl.querySelectorAll<HTMLDetailsElement>("details[open]"),
      (details) => details.dataset.who,
    ),
  );
  if (members.length === 0) {
    const item = document.createElement("li");
    item.className = "members-empty";
    item.textContent = "Nobody has joined yet.";
    membersEl.replaceChildren(item);
  } else {
    membersEl.replaceChildren(
      ...members.map((member) =>
        memberRow(
          member,
          member.who.toText() === mine,
          open.has(member.who.toText()),
        ),
      ),
    );
  }
  membersEl.removeAttribute("aria-busy");
  peopleCountEl.textContent = String(members.length);

  const label = peopleLabel(members.length);
  const summary = (): HTMLElement[] => {
    const text = document.createElement("span");
    text.textContent = label;
    if (members.length === 0) return [text];
    const avatars = document.createElement("span");
    avatars.className = "avatars";
    avatars.append(
      ...members.slice(0, 3).map((member) => avatarOf(member.who, true)),
    );
    return [avatars, text];
  };
  peopleSummaryEl.replaceChildren(...summary());
  peopleOpenBtn.replaceChildren(...summary(), icon("chevron"));
  peopleOpenBtn.setAttribute("aria-label", `${label} in the room: show who`);
};

// ===== Messages =====

/** A message as the list shows it: from the room, or on its way there. */
interface Entry {
  key: string;
  from: Principal;
  at: bigint;
  text: string;
  outgoing?: Outgoing;
}

/** Keys for the room's messages that hold from one read to the next. */
const keysOf = (messages: Message[]): string[] => {
  const seen = new Map<string, number>();
  return messages.map((message) => {
    const base = `${message.at}:${message.from.toText()}`;
    const repeat = seen.get(base) ?? 0;
    seen.set(base, repeat + 1);
    return `${base}:${repeat}`;
  });
};

/**
 * Drops the sent messages the room now holds, which it shows in their place.
 * Their copies in the room are on screen already, so they do not enter again.
 */
const settle = (current: Room) => {
  const mine = me()?.toText();
  if (mine === undefined) return;
  const keys = keysOf(current.messages);
  const taken = new Set<number>();
  outgoing = outgoing.filter((item) => {
    if (item.state !== "sent") return true;
    for (let index = current.messages.length - 1; index >= 0; index--) {
      const message = current.messages[index]!;
      if (item.after !== undefined && message.at < item.after) break;
      if (
        taken.has(index) ||
        message.from.toText() !== mine ||
        message.text !== item.text
      ) {
        continue;
      }
      taken.add(index);
      shownKeys.add(keys[index]!);
      return false;
    }
    return true;
  });
};

/** Whether two messages read as one group: one sender, one day, close in time. */
const together = (earlier: Entry | undefined, later: Entry | undefined) =>
  earlier !== undefined &&
  later !== undefined &&
  earlier.from.toText() === later.from.toText() &&
  dayKey(dateOf(earlier.at)) === dayKey(dateOf(later.at)) &&
  later.at - earlier.at <= GROUP_GAP_NS;

const linkButton = (label: string, act: () => void): HTMLButtonElement => {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "link";
  button.textContent = label;
  button.addEventListener("click", act);
  return button;
};

const textNote = (text: string, symbol?: keyof typeof ICONS): HTMLElement => {
  const note = document.createElement("div");
  note.className = "note";
  if (symbol !== undefined) note.append(icon(symbol));
  note.append(text);
  return note;
};

/** The quiet line under a message of yours: sending, sent, or not sent. */
const noteFor = (entry: Entry, own: boolean, end: boolean) => {
  const item = entry.outgoing;
  if (item === undefined) {
    return own && end ? textNote("Sent", "check") : undefined;
  }
  if (item.state === "sending") return textNote("Sending…");
  if (item.state === "sent") return textNote("Sent", "check");
  const note = document.createElement("div");
  note.className = "note failed";
  const again = linkButton("Retry", () => retry(item));
  again.disabled = sending();
  const separator = () => {
    const dot = document.createElement("span");
    dot.setAttribute("aria-hidden", "true");
    dot.textContent = "·";
    return dot;
  };
  note.append(
    "Not sent",
    separator(),
    again,
    separator(),
    linkButton("Delete", () => discard(item)),
  );
  return note;
};

const messageNodes = (
  entry: Entry,
  own: boolean,
  place: { first: boolean; last: boolean; end: boolean },
  fresh: boolean,
): HTMLElement[] => {
  const row = document.createElement("div");
  row.className = [
    "msg",
    place.first ? "first" : "",
    place.last ? "last" : "",
    fresh ? "enter" : "",
    entry.outgoing?.state ?? "",
  ]
    .filter((name) => name !== "")
    .join(" ");
  row.dataset.key = entry.key;
  const bubble = document.createElement("div");
  bubble.className = "bubble";
  bubble.textContent = entry.text;
  const aside = document.createElement("div");
  aside.className = "aside";
  if (entry.outgoing === undefined) {
    aside.append(timeElement(entry.at), copyButton(entry.text, "Copy message"));
  } else if (entry.outgoing.state === "failed") {
    aside.append(icon("alert"));
  }
  row.append(bubble, aside);
  const note = noteFor(entry, own, place.end);
  return note === undefined ? [row] : [row, note];
};

/** The messages by day, and within a day in groups by sender. */
const timeline = (
  entries: Entry[],
  mine: string | undefined,
  fresh: Set<string>,
): HTMLElement[] => {
  const days: HTMLElement[] = [];
  let day: HTMLElement | undefined;
  let stack: HTMLElement | undefined;
  entries.forEach((entry, index) => {
    const previous = entries[index - 1];
    const next = entries[index + 1];
    const date = dateOf(entry.at);
    const own = entry.from.toText() === mine;
    if (
      day === undefined ||
      previous === undefined ||
      dayKey(dateOf(previous.at)) !== dayKey(date)
    ) {
      day = document.createElement("section");
      day.className = "day";
      day.setAttribute("aria-label", dayLabel(date));
      const label = document.createElement("div");
      label.className = "day-label";
      label.textContent = dayLabel(date);
      day.append(label);
      days.push(day);
    }
    const first = !together(previous, entry);
    if (first || stack === undefined) {
      const group = document.createElement("div");
      group.className = own ? "group mine" : "group theirs";
      if (!own) group.append(avatarOf(entry.from));
      stack = document.createElement("div");
      stack.className = "stack";
      const meta = document.createElement("div");
      meta.className = "group-meta";
      if (!own) {
        const who = document.createElement("span");
        who.className = "who";
        who.textContent = nameOf(entry.from);
        who.title = entry.from.toText();
        meta.append(who);
      }
      meta.append(timeElement(entry.at));
      stack.append(meta);
      group.append(stack);
      day.append(group);
    }
    stack.append(
      ...messageNodes(
        entry,
        own,
        { first, last: !together(entry, next), end: next === undefined },
        fresh.has(entry.key),
      ),
    );
  });
  return days;
};

const atEnd = (): boolean =>
  scrollerEl.scrollHeight - scrollerEl.scrollTop - scrollerEl.clientHeight <
  NEAR_END_PX;

const toEnd = (smooth = false) => {
  scrollerEl.scrollTo({
    top: scrollerEl.scrollHeight,
    behavior: smooth && !reducedMotion.matches ? "smooth" : "auto",
  });
  pinned = true;
  jumpBtn.hidden = true;
};

/** The first message in view and how far down it sits, to keep it there. */
interface Anchor {
  key: string;
  offset: number;
}

const anchorOf = (): Anchor | undefined => {
  const top = scrollerEl.getBoundingClientRect().top;
  for (const row of messagesEl.querySelectorAll<HTMLElement>(".msg")) {
    const box = row.getBoundingClientRect();
    if (box.bottom > top) {
      return { key: row.dataset.key ?? "", offset: box.top - top };
    }
  }
  return undefined;
};

const restore = (anchor: Anchor | undefined) => {
  if (anchor === undefined) return;
  const row = Array.from(messagesEl.querySelectorAll<HTMLElement>(".msg")).find(
    (candidate) => candidate.dataset.key === anchor.key,
  );
  if (row === undefined) return;
  scrollerEl.scrollTop +=
    row.getBoundingClientRect().top -
    scrollerEl.getBoundingClientRect().top -
    anchor.offset;
};

const drawMessages = (current: Room) => {
  const principal = me();
  const mine = principal?.toText();
  const keys = keysOf(current.messages);
  const entries: Entry[] = current.messages.map((message, index) => ({
    key: keys[index]!,
    from: message.from,
    at: message.at,
    text: message.text,
  }));
  if (principal !== undefined) {
    for (const item of outgoing) {
      entries.push({
        key: `out:${item.id}`,
        from: principal,
        at: item.at,
        text: item.text,
        outgoing: item,
      });
    }
  }

  const fresh = loaded
    ? entries.filter((entry) => !shownKeys.has(entry.key))
    : [];
  // A reader at the end, or one who just wrote, wants the end in view; one
  // reading further up keeps their place and is told what came in.
  const stick =
    !loaded || pinned || fresh.some((entry) => entry.outgoing !== undefined);
  const anchor = stick ? undefined : anchorOf();

  messagesEl.replaceChildren(
    ...(entries.length === 0
      ? [
          stateBox(
            "chat",
            "No messages yet",
            client.isAuthenticated()
              ? "Say hello. Everyone in the room gets a notification."
              : "Sign in to join the room and start the conversation.",
          ),
        ]
      : timeline(entries, mine, new Set(fresh.map((entry) => entry.key)))),
  );
  messagesEl.removeAttribute("aria-busy");
  shownKeys = new Set(entries.map((entry) => entry.key));
  loaded = true;

  if (stick) toEnd();
  else restore(anchor);

  const arrivals = fresh.filter(
    (entry) => entry.outgoing === undefined && entry.from.toText() !== mine,
  );
  const latest = arrivals.at(-1);
  if (latest !== undefined) {
    announce(
      arrivals.length === 1
        ? `${nameOf(latest.from)}: ${latest.text}`
        : `${arrivals.length} new messages. ${nameOf(latest.from)}: ${latest.text}`,
    );
    if (!stick) jumpBtn.hidden = false;
  }
};

/** Draws the room and what is on its way to it, unless nothing changed. */
const draw = () => {
  if (room === undefined) return;
  const fingerprint = [
    me()?.toText(),
    client.isAuthenticated(),
    room.members.map((member) => member.who.toText()).join(","),
    room.names.map(([who, name]) => `${who.toText()}=${name}`).join(","),
    room.messages.length,
    room.messages[0]?.at,
    room.messages.at(-1)?.at,
    outgoing.map((item) => `${item.id}:${item.state}`).join(","),
    sending(),
    // Once a day, so "Today" becomes "Yesterday".
    dayKey(new Date()),
  ].join("|");
  if (fingerprint === shownRoom) return;
  shownRoom = fingerprint;
  drawMembers(room);
  drawMessages(room);
};

const renderRoom = (current: Room) => {
  room = current;
  const principal = me()?.toText();
  joined =
    principal !== undefined &&
    current.members.some((member) => member.who.toText() === principal);
  rejoinIfDropped();
  names = new Map(current.names.map(([who, name]) => [who.toText(), name]));
  settle(current);
  draw();
  render();
};

/** Instead of the skeleton, where the first read of the room failed. */
const showUnreachable = () => {
  messagesEl.replaceChildren(
    stateBox(
      "alert",
      "Can't reach the room",
      "Trying again every few seconds.",
    ),
  );
  messagesEl.removeAttribute("aria-busy");
};

// ===== Notification stats =====

const NOTHING: Counts = {
  queued: 0n,
  accepted: 0n,
  received: 0n,
  opened: 0n,
  dropped: 0n,
  deferred: 0n,
};

const add = (into: Counts, from: Counts): Counts => ({
  queued: into.queued + from.queued,
  accepted: into.accepted + from.accepted,
  received: into.received + from.received,
  opened: into.opened + from.opened,
  dropped: into.dropped + from.dropped,
  deferred: into.deferred + from.deferred,
});

const HOUR_NS = 3_600_000_000_000n;

/**
 * A gateway origin as Internet Identity spells it, so that `icp0.io`,
 * `icp.net` and `ic0.app` read as the same app. Anything else stands as it is.
 */
const fold = (origin: string): string =>
  origin.replace(
    /^https:\/\/([a-z0-9-]+)\.(icp0\.io|icp\.net)$/,
    "https://$1.ic0.app",
  );

/**
 * What the canister is set up to do, and what stands in its way. The
 * library's own misconfiguration note is shown only while it still applies:
 * the library writes one when a send hits it and never takes it back.
 */
const configLines = (
  config: NotificationConfig,
  metrics: Metrics,
): { text: string; warning: boolean }[] => {
  const origin = config.origin[0];
  const sender = config.sender[0];
  const lines: { text: string; warning: boolean }[] = [];

  if (origin === undefined) {
    lines.push({
      text: "No origin set, so nothing is sent: put this app's origin into notification_origin in icp.yaml and deploy again.",
      warning: true,
    });
  } else if (sender === undefined) {
    lines.push({
      text: `Sending for ${origin}, but notification_sender names no canister, so nothing is sent.`,
      warning: true,
    });
  } else {
    lines.push({
      text: `Sending for ${origin} through Internet Identity ${sender.toText()}.`,
      warning: false,
    });
  }
  if (origin !== undefined && fold(origin) !== fold(location.origin)) {
    lines.push({
      text: `This page is at ${location.origin}, which is not that origin: Internet Identity records consent for the origin a page signs in from, and looks for the sender list at the one the canister names.`,
      warning: true,
    });
  }
  if (sender !== undefined && sender.toText() !== II_CANISTER_ID) {
    lines.push({
      text: `This page signs in at Internet Identity ${II_CANISTER_ID}, which is not the one the canister sends through: consent given at one is unknown to the other.`,
      warning: true,
    });
  }

  const noted = metrics.misconfigured[0];
  if (noted !== undefined) {
    const why = Object.keys(noted.why)[0];
    const stillStands =
      (why === "Origin" && origin === undefined) ||
      (why === "Sender" && sender === undefined) ||
      why === "NotAuthorizedSender";
    if (stillStands) {
      lines.push({
        text:
          why === "NotAuthorizedSender"
            ? `Internet Identity refused this canister as a sender at ${timeOf(noted.since)}: the origin's /.well-known/ii-notification-senders did not name it. Checked again hourly.`
            : `The library has dropped every send since ${timeOf(noted.since)} for that reason.`,
        warning: true,
      });
    }
  }
  return lines;
};

/** The funnel with one row per stage, so it fits the sidebar's width. */
const renderStats = (metrics: Metrics, config: NotificationConfig) => {
  const now = BigInt(Date.now()) * 1_000_000n;
  const since = (hours: bigint): Counts =>
    metrics.hours
      .filter((bucket) => bucket.start >= now - hours * HOUR_NS)
      .map((bucket) => bucket.counts)
      .reduce(add, NOTHING);

  const configuration = document.createElement("ul");
  configuration.className = "config";
  for (const line of configLines(config, metrics)) {
    const item = document.createElement("li");
    item.className = line.warning ? "warning" : "";
    item.textContent = line.text;
    configuration.append(item);
  }
  const summary = document.createElement("p");
  summary.textContent = `backlog ${metrics.backlog}: notifications waiting for Internet Identity to accept them`;

  const cell = (tag: "th" | "td", text: string, title?: string) => {
    const element = document.createElement(tag);
    element.textContent = text;
    if (title !== undefined) element.title = title;
    return element;
  };
  const windows: [string, string, Counts][] = [
    ["1 h", "last hour", since(1n)],
    ["24 h", "last 24 hours", since(24n)],
    ["30 d", "last 30 days", since(720n)],
  ];
  const stages = [
    "queued",
    "accepted",
    "received",
    "opened",
    "dropped",
    "deferred",
  ] as const;
  const table = document.createElement("table");
  table
    .createTHead()
    .append(
      cell("th", ""),
      ...windows.map(([label, title]) => cell("th", label, title)),
    );
  const body = table.createTBody();
  for (const stage of stages) {
    body
      .insertRow()
      .append(
        cell("th", stage),
        ...windows.map(([, , counts]) => cell("td", String(counts[stage]))),
      );
  }
  body
    .insertRow()
    .append(
      cell("th", "opened/queued"),
      ...windows.map(([, , counts]) =>
        cell(
          "td",
          counts.queued === 0n
            ? "–"
            : `${((Number(counts.opened) / Number(counts.queued)) * 100).toFixed(0)}%`,
        ),
      ),
    );

  statsEl.replaceChildren(configuration, summary, table);
};

// ===== Reading and writing =====

let reader: Promise<ChatService> | undefined;

/**
 * The anonymous actor: queries need no identity. Made once, unless making it
 * failed (a replica still starting, say), when the next poll tries again.
 */
const read = (): Promise<ChatService> =>
  (reader ??= actorFor().catch((error: unknown) => {
    reader = undefined;
    throw error;
  }));

/** An actor as the signed-in identity, for the calls that change the room. */
const write = async (): Promise<ChatService> =>
  actorFor(await client.getIdentity());

const refresh = async () => {
  try {
    const chat = await read();
    renderRoom(await chat.room());
    if (!statsEl.hidden) {
      const [metrics, config] = await Promise.all([
        chat.notification_metrics(),
        chat.notification_config(),
      ]);
      renderStats(metrics, config);
    }
  } catch (error) {
    showStatus(describe(error));
    if (!loaded) showUnreachable();
  }
};

/** Claims a name, and says so where what was given is not what was asked. */
const claim = async (chat: ChatService, wanted: string): Promise<void> => {
  const given = await chat.claim_name(wanted);
  if (given !== wanted) {
    showStatus(`${wanted} is taken, so you are ${given}.`);
  }
};

const whileBusy = async (run: () => Promise<void>) => {
  busy = true;
  showStatus("");
  render();
  try {
    await run();
  } catch (error) {
    showStatus(describe(error));
  } finally {
    busy = false;
    render();
    await refresh();
  }
};

authBtn.addEventListener("click", () => {
  busyLabel = client.isAuthenticated() ? "Signing out…" : "Signing in…";
  void whileBusy(async () => {
    if (client.isAuthenticated()) {
      // Leaving is what stops the notifications, so a failure to leave keeps
      // the sign-in: signed out but still in the room, the user would go on
      // being notified with no way to stop it until the next sign-in.
      try {
        await (await write()).leave();
      } catch (error) {
        throw new Error(
          `Could not leave the room, so you are still signed in; try again. (${describe(error)})`,
        );
      }
      await client.signOut();
      joined = false;
      outgoing = [];
      return;
    }
    // Alongside the sign-in rather than after it: the client closes the
    // channel to Internet Identity once nothing is in flight, and reopening it
    // needs a click this continuation no longer has. Its failure is reported
    // rather than thrown, so an Internet Identity that does not notify (or
    // refuses) still signs the user in.
    const signedIn = client.signIn();
    const consent = client.requestNotificationConsent().then(
      ({ granted }) =>
        granted
          ? undefined
          : "Internet Identity will not notify you here, so you will only see messages on this page.",
      (error) =>
        error instanceof NotificationsUnsupportedError
          ? "This Internet Identity does not send notifications, so you will only see messages on this page."
          : `Asking to notify failed (${describe(error)}), so you will only see messages on this page.`,
    );
    // The name Internet Identity holds for this identity, where it holds one
    // and the user agrees to share it. An identity signed in with a passkey
    // alone has none, and is answered with an empty bundle. A fresh nonce
    // each time: the bundle is read, not verified.
    const named = client
      .requestAttributes({
        keys: ["name"],
        nonce: () =>
          Promise.resolve(crypto.getRandomValues(new Uint8Array(32))),
      })
      .then(
        (signed) => nameAmong(signed.data),
        () => undefined,
      );
    await signedIn;
    const notNotified = await consent;
    const provided = await named;
    const chat = await write();
    await chat.join();
    if (provided !== undefined) {
      await claim(chat, provided);
    }
    if (notNotified !== undefined) {
      showStatus(`${statusEl.textContent ?? ""} ${notNotified}`.trim());
    }
  });
});

/** Sends a message of this page's, and marks it failed if that fails. */
const deliver = (item: Outgoing) => {
  item.state = "sending";
  render();
  draw();
  void (async () => {
    try {
      await (await write()).send(item.text);
      item.state = "sent";
      showStatus("");
    } catch (error) {
      item.state = "failed";
      showStatus(`Not sent: ${describe(error)}`);
    } finally {
      render();
      draw();
      await refresh();
    }
  })();
};

const retry = (item: Outgoing) => {
  if (sending()) return;
  item.after = room?.messages.at(-1)?.at;
  deliver(item);
};

const discard = (item: Outgoing) => {
  outgoing = outgoing.filter((other) => other !== item);
  draw();
};

composeEl.addEventListener("submit", (event) => {
  event.preventDefault();
  const text = textEl.value.trim();
  if (text === "" || sendBtn.disabled) return;
  // The box is cleared at once and stays open, so the next message can be
  // typed while this one is on its way; the button keeps them one at a time.
  textEl.value = "";
  fitText();
  const item: Outgoing = {
    id: outgoingIds++,
    text,
    state: "sending",
    at: BigInt(Date.now()) * 1_000_000n,
    after: room?.messages.at(-1)?.at,
  };
  outgoing.push(item);
  deliver(item);
});

textEl.addEventListener("input", () => {
  fitText();
  render();
});

textEl.addEventListener("keydown", (event) => {
  // Enter sends and Shift+Enter breaks the line; neither while an input
  // method is still composing a character.
  if (
    event.key !== "Enter" ||
    event.shiftKey ||
    event.isComposing ||
    event.keyCode === 229
  ) {
    return;
  }
  event.preventDefault();
  composeEl.requestSubmit();
});

statusDismissBtn.addEventListener("click", () => showStatus(""));

claimEl.addEventListener("submit", (event) => {
  event.preventDefault();
  const wanted = nameEl.value.trim();
  if (wanted === "" || claimBtn.disabled) return;
  claiming = true;
  showStatus("");
  render();
  void (async () => {
    try {
      await claim(await write(), wanted);
      nameEl.value = "";
      closeMenu();
    } catch (error) {
      showStatus(`Not claimed: ${describe(error)}`);
    } finally {
      claiming = false;
      render();
      await refresh();
    }
  })();
});

nameEl.addEventListener("input", render);

statsToggle.addEventListener("click", () => {
  statsEl.hidden = !statsEl.hidden;
  statsToggle.setAttribute("aria-expanded", String(!statsEl.hidden));
  if (!statsEl.hidden && statsEl.childElementCount === 0) {
    statsEl.replaceChildren(
      ...["w-70", "w-55", "w-60"].map((width) => {
        const line = document.createElement("span");
        line.className = `sk sk-line ${width}`;
        return line;
      }),
    );
  }
  void refresh();
});

// ===== Scrolling =====

scrollerEl.addEventListener(
  "scroll",
  () => {
    pinned = atEnd();
    if (pinned) jumpBtn.hidden = true;
  },
  { passive: true },
);

// The list shrinks when the composer grows or the keyboard opens: a reader at
// the end stays at the end.
const keepEnd = new ResizeObserver(() => {
  if (pinned) scrollerEl.scrollTop = scrollerEl.scrollHeight;
});
keepEnd.observe(scrollerEl);
keepEnd.observe(messagesEl);

jumpBtn.addEventListener("click", () => toEnd(true));

/**
 * iOS lays the keyboard over the page rather than shrinking it (Android does
 * shrink it, as the viewport meta asks), so the app is fitted to what is left
 * visible, which keeps the composer above the keyboard. A pinch zoom is left
 * alone.
 */
const fitToViewport = () => {
  const viewport = window.visualViewport;
  if (viewport === null) return;
  const covered =
    Math.abs(viewport.scale - 1) < 0.01 &&
    window.innerHeight - viewport.height > 1;
  appEl.style.height = covered ? `${viewport.height}px` : "";
  appEl.style.transform = covered ? `translateY(${viewport.offsetTop}px)` : "";
};
window.visualViewport?.addEventListener("resize", fitToViewport);
window.visualViewport?.addEventListener("scroll", fitToViewport);

// ===== The people drawer and the account menu =====

const drawerOpen = (): boolean => appEl.classList.contains("drawer-open");

const openPeople = () => {
  appEl.classList.add("drawer-open");
  peopleOpenBtn.setAttribute("aria-expanded", "true");
  peopleEl.setAttribute("role", "dialog");
  peopleEl.setAttribute("aria-modal", "true");
  chatEl.inert = true;
  peopleCloseBtn.focus();
};

const closePeople = (refocus: boolean) => {
  if (!drawerOpen()) return;
  appEl.classList.remove("drawer-open");
  peopleOpenBtn.setAttribute("aria-expanded", "false");
  peopleEl.removeAttribute("role");
  peopleEl.removeAttribute("aria-modal");
  chatEl.inert = false;
  if (refocus) peopleOpenBtn.focus();
};

peopleOpenBtn.addEventListener("click", openPeople);
peopleCloseBtn.addEventListener("click", () => closePeople(true));
scrimEl.addEventListener("click", () => closePeople(true));
document.addEventListener("keydown", (event) => {
  if (event.key === "Escape") closePeople(true);
});
wide.addEventListener("change", () => closePeople(false));

/** Under the account button, right-aligned with it. */
const placeMenu = () => {
  const anchor = accountBtn.getBoundingClientRect();
  menuEl.style.top = `${anchor.bottom + 8}px`;
  menuEl.style.right = `${Math.max(8, document.documentElement.clientWidth - anchor.right)}px`;
};

menuEl.addEventListener("beforetoggle", (event) => {
  if ((event as ToggleEvent).newState === "open") placeMenu();
});
menuEl.addEventListener("toggle", (event) => {
  const open = (event as ToggleEvent).newState === "open";
  accountBtn.classList.toggle("open", open);
  if (open && finePointer.matches) nameEl.focus();
});
window.addEventListener("resize", () => {
  if (menuOpen()) placeMenu();
});

client.subscribe(render);

// Somebody who reloads the page is still signed in, and joins again in case
// an upgrade emptied the room meanwhile.
const start = async () => {
  render();
  fitText();
  fitToViewport();
  if (client.isAuthenticated()) {
    await write()
      .then((chat) => chat.join())
      .catch((error) => showStatus(describe(error)));
  }
  await refresh();
  setInterval(() => void refresh(), POLL_MS);
};

void start();
