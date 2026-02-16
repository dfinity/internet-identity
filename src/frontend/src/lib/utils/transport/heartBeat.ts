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

export class HeartbeatServer {
  readonly #options: Required<HeartbeatServerOptions>;

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
  }
}
