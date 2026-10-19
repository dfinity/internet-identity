import {
  type Channel,
  type ChannelOptions,
  type JsonRequest,
  JsonRequestSchema,
  type JsonResponse,
  type Transport,
} from "$lib/utils/transport/utils";
import { HeartbeatServer } from "./heartBeat";

const ESTABLISH_TIMEOUT_MS = 10000;
const DISCONNECT_TIMEOUT_MS = 10000;

export class PostMessageUnsupportedError extends Error {}

interface PostMessageChannelOptions {
  // In-flight requests to seed the channel with, replayed to each `request`
  // listener as it subscribes (as a live request would be). Used to resume an
  // authorize request that arrived before an II-internal redirect, on the
  // channel re-established on return.
  pendingRequests?: JsonRequest[];
  // Switches the underlying heartbeat to `pending` and resolves once the client
  // has observed it; see {@link HeartbeatServer.setPending}.
  markPending?: () => Promise<void>;
}

class PostMessageChannel implements Channel {
  // Carried so the authorize page can stash it before a 1-click redirect, but
  // not gated on here: this transport re-handshakes with the opener (scoped by
  // `allowedOrigin`), not by resuming a persisted flow. Transports that do
  // resume a persisted flow gate on it.
  readonly resumeToken = crypto.randomUUID();
  #origin: string;
  #source: WindowProxy;
  #windowListener: (event: MessageEvent) => void;
  #closed = false;
  #requests: JsonRequest[];
  #requestListeners = new Set<(request: JsonRequest) => void>();
  #closeListeners = new Set<() => void>();
  #markPending?: () => Promise<void>;

  constructor(
    origin: string,
    source: WindowProxy,
    options: PostMessageChannelOptions = {},
  ) {
    this.#origin = origin;
    this.#source = source;
    // Seed with any carried-over requests so a `request` listener subscribing
    // after a redirect-resume replays them, exactly as it would a live one.
    this.#requests = [...(options.pendingRequests ?? [])];
    this.#markPending = options.markPending;
    this.#windowListener = (event: MessageEvent) => {
      const { data, success } = JsonRequestSchema.safeParse(event.data);
      if (
        event.source !== this.#source ||
        event.origin !== this.#origin ||
        !success ||
        data?.method === "icrc29_status"
      ) {
        return;
      }
      this.#requests.push(data);
      this.#requestListeners.forEach((listener) => listener(data));
    };

    window.addEventListener("message", this.#windowListener);
  }

  get origin() {
    return this.#origin;
  }

  get closed() {
    return this.#closed;
  }

  get pendingRequests(): JsonRequest[] {
    return [...this.#requests];
  }

  markPending(): Promise<void> {
    return this.#markPending?.() ?? Promise.resolve();
  }

  addEventListener(
    ...[event, listener]:
      | [event: "close", listener: () => void]
      | [event: "request", listener: (request: JsonRequest) => void]
  ): () => void {
    switch (event) {
      case "close":
        this.#closeListeners.add(listener);
        return () => {
          this.#closeListeners.delete(listener);
        };
      case "request": {
        // Replay requests that didn't get a response yet to new listener
        this.#requests.forEach((request) => listener(request));
        this.#requestListeners.add(listener);
        return () => {
          this.#requestListeners.delete(listener);
        };
      }
    }
  }

  send(response: JsonResponse): Promise<void> {
    if (this.#closed) {
      throw new Error("Post message channel is closed");
    }
    // Send response and remove request
    this.#source.postMessage(response, this.#origin);
    this.#requests = this.#requests.filter(
      (request) => request.id !== response.id,
    );
    return Promise.resolve();
  }

  close(): Promise<void> {
    window.removeEventListener("message", this.#windowListener);

    this.#closed = true;
    this.#closeListeners.forEach((listener) => listener());
    return Promise.resolve();
  }
}

export class PostMessageTransport implements Transport {
  establishChannel(options: ChannelOptions): Promise<PostMessageChannel> {
    // Detect X's in-app browser and reject immediately so we don't
    // leave users waiting if the channel cannot be established due
    // to X's restrictions on postMessage.
    const isX = /\bTwitter/i.test(navigator.userAgent);
    if (isX) {
      throw new PostMessageUnsupportedError();
    }

    return new Promise((resolve, reject) => {
      let channel: PostMessageChannel;
      const heartbeat = new HeartbeatServer({
        status: options?.pending === true ? "pending" : "ready",
        allowedOrigin: options?.allowedOrigin,
        establishTimeout: ESTABLISH_TIMEOUT_MS,
        disconnectTimeout: DISCONNECT_TIMEOUT_MS,
        onEstablish: (origin, source) => {
          channel = new PostMessageChannel(origin, source, {
            pendingRequests: options?.pendingRequests,
            // Lets the authorize page hold the RP open across an OpenID/SSO hop
            // by switching this channel to `pending` before it navigates away.
            markPending: () => heartbeat.setPending(),
          });
          resolve(channel);
        },
        onEstablishTimeout(): void {
          reject(new Error("Post message channel could not be established"));
        },
        onDisconnect: () => {
          void channel.close();
        },
      });
    });
  }
}
