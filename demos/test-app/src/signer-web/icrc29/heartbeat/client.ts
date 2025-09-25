import { isJsonRpcResponse, type JsonResponse } from "@slide-computer/signer";

export interface HeartbeatClientOptions {
  /**
   * Signer window to send and receive heartbeat messages from
   */
  signerWindow: Window;
  /**
   * Callback when first heartbeat has been received
   */
  onEstablish: (origin: string) => void;
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
   * Time in milliseconds of not receiving heartbeat responses after which the communication channel is disconnected
   * @default 2000
   */
  disconnectTimeout?: number;
  /**
   * Callback when no heartbeats have been received for {@link disconnectTimeout} milliseconds
   */
  onDisconnect: () => void;
  /**
   * Status polling rate in ms
   * @default 300
   */
  statusPollingRate?: number;
  /**
   * Relying party window, used to listen for incoming message events
   * @default globalThis.window
   */
  window?: Window;
  /**
   * Get random uuid implementation for status messages
   * @default globalThis.crypto
   */
  crypto?: Pick<Crypto, "randomUUID">;
}

export class HeartbeatClient {
  readonly #options: Required<HeartbeatClientOptions>;

  constructor(options: HeartbeatClientOptions) {
    this.#options = {
      establishTimeout: 10000,
      disconnectTimeout: 2000,
      statusPollingRate: 300,
      window: globalThis.window,
      crypto: globalThis.crypto,
      ...options,
    };

    this.#establish();
  }

  #establish(): void {
    let pending: Array<string | number> = [];

    // Create new pending entry that's waiting for a response
    const create = (): string => {
      const id = this.#options.crypto.randomUUID();
      pending.push(id);
      return id;
    };

    // Establish communication channel if a response is received for any pending id
    const listener = this.#receiveReadyResponse((response) => {
      if (pending.includes(response.data.id)) {
        pending = [];
        listener();
        clearInterval(interval);
        clearTimeout(timeout);

        this.#options.onEstablish(response.origin);
        this.#maintain(response.origin);
      }
    });

    // Init timeout
    const timeout = setTimeout(() => {
      listener();
      clearInterval(interval);

      this.#options.onEstablishTimeout();
    }, this.#options.establishTimeout);

    // Start sending requests
    const interval = setInterval(
      () => this.#sendStatusRequest(create()),
      this.#options.statusPollingRate,
    );
  }

  #maintain(origin: string): void {
    let interval: ReturnType<typeof setInterval>;
    let timeout: ReturnType<typeof setTimeout>;
    let pending: Array<{ id: string | number; time: number }> = [];

    // Consume a pending entry if it exists
    const consume = (id: string | number): boolean => {
      const index = pending.findIndex((entry) => entry.id === id);
      if (index > -1) {
        pending.splice(index, 1);
      }
      return index > -1;
    };

    // Create new pending entry that's waiting for a response
    const create = (): string => {
      const id = this.#options.crypto.randomUUID();
      const time = new Date().getTime();

      // Cleanup ids outside disconnect window
      pending = pending.filter(
        (entry) => time - this.#options.disconnectTimeout > entry.time,
      );

      // Insert and return new id
      pending.push({ id, time });
      return id;
    };

    // Clear existing timeout (if any) and create a new one
    const resetTimeout = () => {
      clearTimeout(timeout);
      timeout = setTimeout(() => {
        listener();
        clearInterval(interval);

        this.#options.onDisconnect();
      }, this.#options.disconnectTimeout);
    };

    // Reset disconnect timeout if a response is received to an id within disconnect window
    const listener = this.#receiveReadyResponse((response) => {
      if (response.origin === origin && consume(response.data.id)) {
        resetTimeout();
      }
    });

    // Init timeout and start sending requests
    resetTimeout();
    interval = setInterval(
      () => this.#sendStatusRequest(create()),
      this.#options.statusPollingRate,
    );
  }

  #receiveReadyResponse(
    handler: (event: MessageEvent<JsonResponse<"ready">>) => void,
  ): () => void {
    const listener = (event: MessageEvent) => {
      if (
        event.source === this.#options.signerWindow &&
        isJsonRpcResponse(event.data) &&
        "result" in event.data &&
        event.data.result === "ready"
      ) {
        handler(event);
      }
    };
    this.#options.window.addEventListener("message", listener);
    return () => this.#options.window.removeEventListener("message", listener);
  }

  #sendStatusRequest(id: string): void {
    this.#options.signerWindow.postMessage(
      { jsonrpc: "2.0", id, method: "icrc29_status" },
      "*",
    );
  }
}
