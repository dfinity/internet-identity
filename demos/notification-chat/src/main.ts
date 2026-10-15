// The whole frontend: a sign-in button that also asks Internet Identity
// whether this app may notify, the room read every few seconds, a line to type
// in, and a panel with the notification library's delivery stats.

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

interface ChatService {
  join: () => Promise<void>;
  leave: () => Promise<void>;
  send: (text: string) => Promise<void>;
  room: () => Promise<Room>;
  notification_metrics: () => Promise<Metrics>;
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
    room: IDL.Func([], [Room], ["query"]),
    notification_metrics: IDL.Func([], [Metrics], ["query"]),
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

// ===== The page =====

const element = <T extends HTMLElement>(id: string): T => {
  const found = document.getElementById(id);
  if (found === null) throw new Error(`no #${id}`);
  return found as T;
};

const authBtn = element<HTMLButtonElement>("auth");
const meEl = element<HTMLSpanElement>("me");
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
  meEl.textContent = principal === undefined ? "" : short(principal);
  textEl.disabled = !(signedIn && joined);
  textEl.placeholder = signedIn
    ? joined
      ? "Message the room"
      : "Joining…"
    : "Sign in to chat";
  sendBtn.textContent = sending ? "Sending…" : "Send";
  sendBtn.disabled = textEl.disabled || sending || textEl.value.trim() === "";
};

const renderRoom = (room: Room) => {
  const principal = me()?.toText();
  joined =
    principal !== undefined &&
    room.members.some((member) => member.who.toText() === principal);

  // Left alone when nothing changed: rebuilding the lists on every poll would
  // have assistive technology read them out again. Messages arrive at the end
  // and leave at the start, so the ends and the count name the list.
  const newest = room.messages.at(-1)?.at;
  const fingerprint = [
    principal,
    room.members.map((member) => member.who.toText()).join(","),
    room.messages.length,
    room.messages[0]?.at,
    newest,
  ].join("|");
  if (fingerprint === shownRoom) {
    render();
    return;
  }
  shownRoom = fingerprint;

  membersEl.replaceChildren(
    ...room.members.map((member) => {
      const item = document.createElement("li");
      const mine = member.who.toText() === principal;
      item.textContent = `${short(member.who)}${mine ? " (you)" : ""}`;
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
        who.textContent = mine ? "you" : short(message.from);
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

const renderStats = (metrics: Metrics) => {
  const now = BigInt(Date.now()) * 1_000_000n;
  const since = (hours: bigint): Counts =>
    metrics.hours
      .filter((bucket) => bucket.start >= now - hours * HOUR_NS)
      .map((bucket) => bucket.counts)
      .reduce(add, NOTHING);

  const misconfigured = metrics.misconfigured[0];
  const summary = document.createElement("p");
  summary.textContent =
    `backlog ${metrics.backlog} · ` +
    (misconfigured === undefined
      ? "configured"
      : `misconfigured: ${Object.keys(misconfigured.why)[0]} since ${timeOf(misconfigured.since)}`);

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

  statsEl.replaceChildren(summary, table);
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
      renderStats(await chat.notification_metrics());
    }
  } catch (error) {
    showStatus(describe(error));
  }
};

const whileBusy = async (run: () => Promise<void>) => {
  busy = true;
  render();
  try {
    await run();
    showStatus("");
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
        granted ? undefined : "Internet Identity will not notify you here",
      (error) => `asking to notify failed: ${describe(error)}`,
    );
    await signedIn;
    const notNotified = await consent;
    await (await write()).join();
    if (notNotified !== undefined) {
      throw new Error(
        `${notNotified}, so you will only see messages on this page.`,
      );
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
