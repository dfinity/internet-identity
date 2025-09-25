import {
  type Channel,
  isJsonRpcResponse,
  type JsonRequest,
  type JsonResponse,
} from "@slide-computer/signer";
import { PostMessageTransportError } from "./postMessageTransport";

export interface PostMessageChannelOptions {
  /**
   * Signer window with which a communication channel has been established
   */
  signerWindow: Window;
  /**
   * Signer origin obtained when communication channel was established
   */
  signerOrigin: string;
  /**
   * Relying party window, used to listen for incoming message events
   * @default globalThis.window
   */
  window?: Window;
  /**
   * Manage focus between relying party and signer window
   * @default true
   */
  manageFocus?: boolean;
}

export class PostMessageChannel implements Channel {
  readonly #closeListeners = new Set<() => void>();
  readonly #options: Required<PostMessageChannelOptions>;
  #closed = false;

  constructor(options: PostMessageChannelOptions) {
    this.#options = {
      window: globalThis.window,
      manageFocus: true,
      ...options,
    };
  }

  get closed() {
    return this.#closed;
  }

  addEventListener(
    ...[event, listener]:
      | [event: "close", listener: () => void]
      | [event: "response", listener: (response: JsonResponse) => void]
  ): () => void {
    switch (event) {
      case "close":
        this.#closeListeners.add(listener);
        return () => {
          this.#closeListeners.delete(listener);
        };
      case "response":
        const messageListener = async (event: MessageEvent) => {
          if (
            event.source !== this.#options.signerWindow ||
            event.origin !== this.#options.signerOrigin ||
            !isJsonRpcResponse(event.data)
          ) {
            return;
          }
          listener(event.data);
        };
        this.#options.window.addEventListener("message", messageListener);
        return () => {
          this.#options.window.removeEventListener("message", messageListener);
        };
    }
  }

  async send(request: JsonRequest): Promise<void> {
    if (this.#closed) {
      throw new PostMessageTransportError("Communication channel is closed");
    }

    this.#options.signerWindow.postMessage(request, this.#options.signerOrigin);

    if (this.#options.manageFocus) {
      this.#options.signerWindow.focus();
    }
  }

  async close(): Promise<void> {
    if (this.#closed) {
      return;
    }

    this.#closed = true;

    this.#options.signerWindow.close();
    if (this.#options.manageFocus) {
      this.#options.window.focus();
    }

    this.#closeListeners.forEach((listener) => listener());
  }
}
