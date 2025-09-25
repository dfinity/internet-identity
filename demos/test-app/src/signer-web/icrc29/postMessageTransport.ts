import { type Transport } from "@slide-computer/signer";
import { PostMessageChannel } from "./postMessageChannel";
import { urlIsSecureContext } from "../utils";
import { HeartbeatClient } from "./heartbeat";

const NON_CLICK_ESTABLISHMENT_LINK =
  "https://github.com/slide-computer/signer-js/blob/main/packages/signer-web/README.md#channels-must-be-established-in-a-click-handler";

export class PostMessageTransportError extends Error {
  constructor(message: string) {
    super(message);
    Object.setPrototypeOf(this, PostMessageTransportError.prototype);
  }
}

export interface PostMessageTransportOptions {
  /**
   * Signer RPC url to send and receive messages from
   */
  url: string;
  /**
   * Signer window feature config string
   * @example "toolbar=0,location=0,menubar=0,width=500,height=500,left=100,top=100"
   */
  windowOpenerFeatures?: string;
  /**
   * Relying party window, used to listen for incoming message events
   * @default globalThis.window
   */
  window?: Window;
  /**
   * Reasonable time in milliseconds in which the communication channel needs to be established
   * TODO: Lower this value once "not available, try again later" error is standardized and implemented
   * @default 120000
   */
  establishTimeout?: number;
  /**
   * Time in milliseconds of not receiving heartbeat responses after which the communication channel is disconnected
   * @default 2000
   */
  disconnectTimeout?: number;
  /**
   * Status polling rate in ms
   * @default 300
   */
  statusPollingRate?: number;
  /**
   * Get random uuid implementation for status messages
   * @default globalThis.crypto
   */
  crypto?: Pick<Crypto, "randomUUID">;
  /**
   * Manage focus between relying party and signer window
   * @default true
   */
  manageFocus?: boolean;
  /**
   * Close signer window on communication channel establish timeout
   * @default true
   */
  closeOnEstablishTimeout?: boolean;
  /**
   * Detect attempts to establish channel outside of click handler
   * @default true
   */
  detectNonClickEstablishment?: boolean;
}

// Boolean that tracks click events to check if the popup is opened within a click context
let withinClick = false;
if (globalThis.window) {
  globalThis.window.addEventListener("click", () => (withinClick = true), true);
  globalThis.window.addEventListener("click", () => (withinClick = false));
}

export class PostMessageTransport implements Transport {
  readonly #options: Required<PostMessageTransportOptions>;

  constructor(options: PostMessageTransportOptions) {
    if (!urlIsSecureContext(options.url)) {
      throw new PostMessageTransportError("Invalid signer RPC url");
    }

    this.#options = {
      windowOpenerFeatures: "",
      window: globalThis.window,
      establishTimeout: 120000,
      disconnectTimeout: 2000,
      statusPollingRate: 300,
      crypto: globalThis.crypto,
      manageFocus: true,
      closeOnEstablishTimeout: true,
      detectNonClickEstablishment: true,
      ...options,
    };
  }

  async establishChannel(): Promise<PostMessageChannel> {
    if (this.#options.detectNonClickEstablishment && !withinClick) {
      throw new PostMessageTransportError(
        `Signer window should not be opened outside of click handler, see: ${NON_CLICK_ESTABLISHMENT_LINK}`,
      );
    }
    const signerWindow = this.#options.window.open(
      this.#options.url,
      "signerWindow",
      this.#options.windowOpenerFeatures,
    );
    if (!signerWindow) {
      throw new PostMessageTransportError("Signer window could not be opened");
    }

    return new Promise<PostMessageChannel>((resolve, reject) => {
      let channel: PostMessageChannel;
      new HeartbeatClient({
        ...this.#options,
        signerWindow,
        onEstablish: (origin) => {
          channel = new PostMessageChannel({
            ...this.#options,
            signerOrigin: origin,
            signerWindow: signerWindow,
          });
          resolve(channel);
        },
        onEstablishTimeout: () => {
          if (this.#options.closeOnEstablishTimeout) {
            signerWindow.close();
          }
          reject(
            new PostMessageTransportError(
              "Communication channel could not be established within a reasonable time",
            ),
          );
        },
        onDisconnect: () => channel.close(),
      });
    });
  }
}
