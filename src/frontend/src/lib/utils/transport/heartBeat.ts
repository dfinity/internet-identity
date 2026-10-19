import { JsonRequestSchema } from "./utils";

export interface HeartbeatServerOptions {
  /**
   * The initial server status to return to the client
   * @default "ready"
   */
  status?: "pending" | "ready";
  /**
   * The allowed origin that the communication channel can be established with, recommended for secure re-establishment
   */
  allowedOrigin?: string | null;
  /**
   * Callback when first heartbeat has been received
   */
  onEstablish: (origin: string, source: WindowProxy) => void;
  /**
   * Reasonable time in milliseconds in which the communication channel needs to be established
   * @default 10000
   */
  establishTimeout?: number;
  /**
   * Callback when no heartbeats have been received for {@link establishTimeout} milliseconds
   */
  onEstablishTimeout: () => void;
  /**
   * Time in milliseconds of not receiving heartbeat requests after which the communication channel is disconnected
   * @default 2000
   */
  disconnectTimeout?: number;
  /**
   * Callback when no heartbeats have been received for {@link disconnectTimeout} milliseconds
   */
  onDisconnect: () => void;
  /**
   * Signer window, used to listen for incoming message events
   * @default globalThis.window
   */
  window?: Window;
}

/**
 * Upper bound on how long {@link HeartbeatServer.setPending} waits for the
 * client to poll and observe the new `pending` status before resolving anyway.
 * The ICRC-29 client polls a few times a second, so a pending response normally
 * reaches it well inside this; the cap only keeps the caller's redirect from
 * hanging if the client has stopped polling (e.g. it already disconnected).
 */
const PENDING_ACK_TIMEOUT_MS = 1000;

export class HeartbeatServer {
  readonly #options: Required<HeartbeatServerOptions>;
  // Resolver for a {@link setPending} awaiting the client to observe `pending`;
  // run (and cleared) the next time a status response is sent while pending.
  #pendingAck?: () => void;

  constructor(options: HeartbeatServerOptions) {
    this.#options = {
      status: "ready",
      establishTimeout: 10000,
      disconnectTimeout: 2000,
      window: globalThis.window,
      ...options,
      allowedOrigin: options.allowedOrigin ?? null,
    };

    this.#establish();
  }

  changeStatus(status: "pending" | "ready"): void {
    this.#options.status = status;
  }

  /**
   * Switch the channel to `pending` and resolve once the client has been
   * answered with it at least once — i.e. once the client knows to wait rather
   * than give up. The ICRC-29 client holds a `pending` channel open far longer
   * than a `ready` one, so this is what lets II navigate away for an internal
   * redirect (an OpenID/SSO hop) without the client's short ready-timeout
   * closing the window mid-flow. Resolves on a {@link PENDING_ACK_TIMEOUT_MS}
   * fallback if no poll arrives, so a redirect is never blocked indefinitely.
   */
  setPending(): Promise<void> {
    this.#options.status = "pending";
    return new Promise<void>((resolve) => {
      let settled = false;
      const done = () => {
        if (settled) {
          return;
        }
        settled = true;
        clearTimeout(timer);
        if (this.#pendingAck === done) {
          this.#pendingAck = undefined;
        }
        resolve();
      };
      this.#pendingAck = done;
      const timer = setTimeout(done, PENDING_ACK_TIMEOUT_MS);
    });
  }

  #establish(): void {
    // Establish communication channel if a request is received
    const listener = this.#receiveStatusRequest((request) => {
      if (request.source === null || request.data.id === undefined) {
        return;
      }
      listener();
      clearTimeout(timeout);

      this.#options.onEstablish(request.origin, request.source as WindowProxy);
      this.#sendStatusResponse(request.data.id, request.origin, request.source);
      this.#maintain(request.origin, request.source);
    });

    // Send initial status response to kickstart the process
    // in case the client is already waiting for responses.
    //
    // This resumes the client page from sleep in the case
    // of browsers like Safari that unload background pages
    // after a certain idle time and requiring an event to
    // wake them up again (e.g. by sending a message to them).
    if (
      this.#options.allowedOrigin !== null &&
      this.#options.window.opener !== null
    ) {
      this.#sendStatusResponse(
        "wake-up-client",
        this.#options.allowedOrigin,
        this.#options.window.opener,
      );
    }

    // Init timeout
    const timeout = setTimeout(() => {
      listener();

      this.#options.onEstablishTimeout();
    }, this.#options.establishTimeout);
  }

  #maintain(origin: string, source: MessageEventSource): void {
    let timeout: ReturnType<typeof setTimeout>;

    // Clear existing timeout (if any) and create a new one
    const resetTimeout = () => {
      clearTimeout(timeout);
      timeout = setTimeout(() => {
        listener();

        this.#options.onDisconnect();
      }, this.#options.disconnectTimeout);
    };

    // Init timeout and start sending messages
    resetTimeout();

    // Reset disconnect timeout and send response if a request is received
    const listener = this.#receiveStatusRequest((request) => {
      if (
        request.origin === origin &&
        request.source === source &&
        request.data.id !== undefined
      ) {
        resetTimeout();
        this.#sendStatusResponse(
          request.data.id,
          request.origin,
          request.source,
        );
      }
    });
  }

  #receiveStatusRequest(handler: (event: MessageEvent) => void): () => void {
    const listener = (event: MessageEvent) => {
      const result = JsonRequestSchema.safeParse(event.data);
      if (
        !result.success ||
        result.data.method !== "icrc29_status" ||
        (this.#options.allowedOrigin !== null &&
          event.origin !== this.#options.allowedOrigin)
      ) {
        return;
      }
      handler(event);
    };
    this.#options.window.addEventListener("message", listener);
    return () => this.#options.window.removeEventListener("message", listener);
  }

  #sendStatusResponse(
    id: string | number,
    origin: string,
    source: MessageEventSource,
  ): void {
    (source as WindowProxy).postMessage(
      { jsonrpc: "2.0", id, result: this.#options.status },
      origin,
    );
    // A `setPending` caller waits for the client to have been told to wait; this
    // response is that moment. Only `pending` counts: a later `ready` response
    // must not resolve a wait meant to confirm the client saw `pending`.
    if (this.#options.status === "pending") {
      this.#pendingAck?.();
    }
  }
}
