import { isJsonRpcRequest, type JsonRequest } from "@slide-computer/signer";

export interface HeartbeatServerOptions {
  /**
   * Callback when first heartbeat has been received
   */
  onEstablish: (origin: string, source: MessageEventSource) => void;
  /**
   * Reasonable time in milliseconds in which the communication channel needs to be established
   * @default 2000
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
      establishTimeout: 10000,
      disconnectTimeout: 2000,
      window: globalThis.window,
      ...options,
    };

    this.#establish();
  }

  #establish(): void {
    // Establish communication channel if a request is received
    const listener = this.#receiveStatusRequest((request) => {
      if (!request.source) {
        return;
      }
      listener();
      clearTimeout(timeout);

      this.#options.onEstablish(request.origin, request.source);
      this.#maintain(request.origin, request.source);
    });

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
        this.#sendReadyResponse(request.data.id, request.source);
      }
    });
  }

  #receiveStatusRequest(
    handler: (event: MessageEvent<JsonRequest<"icrc29_status">>) => void,
  ): () => void {
    const listener = (event: MessageEvent) => {
      if (
        isJsonRpcRequest(event.data) &&
        event.data.method === "icrc29_status"
      ) {
        handler(event);
      }
    };
    this.#options.window.addEventListener("message", listener);
    return () => this.#options.window.removeEventListener("message", listener);
  }

  #sendReadyResponse(id: string | number, source: MessageEventSource): void {
    source.postMessage({ jsonrpc: "2.0", id, result: "ready" });
  }
}
