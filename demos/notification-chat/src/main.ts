// The whole frontend: a sign-in button that also asks Internet Identity
// whether this app may notify and what the user is called, the room read every
// few seconds, a line to type in, a line to claim a name on, and a panel with
// the notification library's delivery stats.

import { AuthClient } from "@icp-sdk/auth/client";
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

const authBtn = element<HTMLButtonElement>("auth");
const meEl = element<HTMLSpanElement>("me");
const claimEl = element<HTMLFormElement>("claim");
const nameEl = element<HTMLInputElement>("name");
const claimBtn = element<HTMLButtonElement>("claim-btn");
const messagesEl = element<HTMLDivElement>("messages");
const membersEl = element<HTMLUListElement>("members");
const composeEl = element<HTMLFormElement>("compose");
const textEl = element<HTMLInputElement>("text");
const sendBtn = element<HTMLButtonElement>("send");
const statusEl = element<HTMLParagraphElement>("status");
const statsToggle = element<HTMLButtonElement>("stats-toggle");
const statsEl = element<HTMLDivElement>("stats");

const client = new AuthClient({
  identityProvider: { authorizeUrl: II_URL, canisterId: II_CANISTER_ID },
  agentOptions: { host: location.origin, shouldFetchRootKey: fetchRootKey },
});

/** Whether this identity is in the room, as the last read of it said. */
let joined = false;
let busy = false;
/** A send in flight: one goes at a time, and the line stays open meanwhile. */
let sending = false;
/** A claim in flight, likewise. */
let claiming = false;
/** A join in flight, so a room that says we are out is not joined twice. */
let rejoining = false;
/** Who is called what, by principal, as the last read of the room said. */
let names = new Map<string, string>();
/** The room as it was last drawn, so an unchanged room is left alone. */
let shownRoom = "";
/** When the newest message drawn arrived, so a newer one scrolls into view. */
let newestShown: bigint | undefined;

const me = (): Principal | undefined => client.getPrincipal();

/** A principal as it is shown: its first and last groups. */
const short = (principal: Principal): string => {
  const groups = principal.toText().split("-");
  return groups.length > 1 ? `${groups[0]}…${groups.at(-1)}` : groups[0]!;
};

/** Somebody as they are shown: their claimed name, or their principal's ends. */
const nameOf = (principal: Principal): string =>
  names.get(principal.toText()) ?? short(principal);

const timeOf = (nanos: bigint): string =>
  new Date(Number(nanos / 1_000_000n)).toLocaleTimeString();

const describe = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const showStatus = (text: string) => {
  statusEl.textContent = text;
};

const render = () => {
  const signedIn = client.isAuthenticated();
  const principal = me();
  authBtn.textContent = busy ? "…" : signedIn ? "Sign out" : "Sign in";
  authBtn.disabled = busy;
  meEl.textContent = principal === undefined ? "" : nameOf(principal);
  meEl.title = principal === undefined ? "" : principal.toText();
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
  sendBtn.textContent = sending ? "Sending…" : "Send";
  sendBtn.disabled = textEl.disabled || sending || textEl.value.trim() === "";
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

const renderRoom = (room: Room) => {
  const principal = me()?.toText();
  joined =
    principal !== undefined &&
    room.members.some((member) => member.who.toText() === principal);
  rejoinIfDropped();

  // Left alone when nothing changed: rebuilding the lists on every poll would
  // have assistive technology read them out again. Messages arrive at the end
  // and leave at the start, so the ends and the count name the list.
  const newest = room.messages.at(-1)?.at;
  const fingerprint = [
    principal,
    room.members.map((member) => member.who.toText()).join(","),
    room.names.map(([who, name]) => `${who.toText()}=${name}`).join(","),
    room.messages.length,
    room.messages[0]?.at,
    newest,
  ].join("|");
  if (fingerprint === shownRoom) {
    render();
    return;
  }
  shownRoom = fingerprint;
  names = new Map(room.names.map(([who, name]) => [who.toText(), name]));

  membersEl.replaceChildren(
    ...room.members.map((member) => {
      const item = document.createElement("li");
      const mine = member.who.toText() === principal;
      item.textContent = `${nameOf(member.who)}${mine ? " (you)" : ""}`;
      item.title = member.who.toText();
      return item;
    }),
  );
  if (room.members.length === 0) {
    const item = document.createElement("li");
    item.className = "empty";
    item.textContent = "nobody yet";
    membersEl.append(item);
  }

  if (room.messages.length === 0) {
    const empty = document.createElement("p");
    empty.className = "empty";
    empty.textContent = "No messages yet.";
    messagesEl.replaceChildren(empty);
  } else {
    messagesEl.replaceChildren(
      ...room.messages.map((message) => {
        const mine = message.from.toText() === principal;
        const row = document.createElement("div");
        row.className = mine ? "message mine" : "message";
        const who = document.createElement("span");
        who.className = "who";
        who.textContent = nameOf(message.from);
        who.title = message.from.toText();
        const when = document.createElement("span");
        when.className = "when";
        when.textContent = timeOf(message.at);
        const text = document.createElement("span");
        text.className = "text";
        text.textContent = message.text;
        row.append(who, when, text);
        return row;
      }),
    );
  }
  // Something new is at the bottom, so that is where to look.
  if (newest !== newestShown) {
    newestShown = newest;
    messagesEl.scrollTop = messagesEl.scrollHeight;
  }
  render();
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

  const stages = [
    "queued",
    "accepted",
    "received",
    "opened",
    "dropped",
    "deferred",
  ] as const;
  const table = document.createElement("table");
  const head = table.createTHead().insertRow();
  head.insertCell().textContent = "window";
  for (const stage of stages) head.insertCell().textContent = stage;
  head.insertCell().textContent = "opened/queued";
  const body = table.createTBody();
  const windows: [string, Counts][] = [
    ["last hour", since(1n)],
    ["last 24 h", since(24n)],
    ["last 30 d", since(720n)],
  ];
  for (const [label, counts] of windows) {
    const row = body.insertRow();
    row.insertCell().textContent = label;
    for (const stage of stages)
      row.insertCell().textContent = String(counts[stage]);
    row.insertCell().textContent =
      counts.queued === 0n
        ? "–"
        : `${((Number(counts.opened) / Number(counts.queued)) * 100).toFixed(0)}%`;
  }

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
      return;
    }
    // Alongside the sign-in rather than after it: the client closes the
    // channel to Internet Identity once nothing is in flight, and reopening it
    // needs a click this continuation no longer has. Its failure is reported
    // rather than thrown, so an Internet Identity that does not notify (or
    // refuses) still signs the user in.
    const signedIn = client.signIn();
    const consent = client.requestNotificationConsent().then(
      (granted) =>
        granted
          ? undefined
          : "Internet Identity will not notify you here, so you will only see messages on this page.",
      (error) =>
        `Asking to notify failed (${describe(error)}), so you will only see messages on this page.`,
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

composeEl.addEventListener("submit", (event) => {
  event.preventDefault();
  const text = textEl.value.trim();
  if (text === "" || sendBtn.disabled) return;
  // The line is cleared at once and stays open, so the next message can be
  // typed while this one is on its way; the button keeps them one at a time.
  sending = true;
  textEl.value = "";
  render();
  void (async () => {
    try {
      await (await write()).send(text);
      showStatus("");
    } catch (error) {
      showStatus(`Not sent: ${describe(error)}`);
      // Handed back to the line, where nothing else has been typed meanwhile.
      if (textEl.value === "") textEl.value = text;
    } finally {
      sending = false;
      render();
      await refresh();
    }
  })();
});

textEl.addEventListener("input", render);

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
  statsToggle.textContent = statsEl.hidden
    ? "Show notification stats"
    : "Hide notification stats";
  void refresh();
});

client.subscribe(render);

// Somebody who reloads the page is still signed in, and joins again in case
// an upgrade emptied the room meanwhile.
const start = async () => {
  render();
  if (client.isAuthenticated()) {
    await write()
      .then((chat) => chat.join())
      .catch((error) => showStatus(describe(error)));
  }
  await refresh();
  setInterval(() => void refresh(), POLL_MS);
};

void start();
