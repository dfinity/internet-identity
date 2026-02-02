import {
  type Channel,
  type ChannelOptions,
  type JsonRequest,
  JsonRequestSchema,
  type JsonResponse,
  type Transport,
} from "$lib/utils/transport/utils";
import { HeartbeatServer } from "@slide-computer/signer-web";

const ESTABLISH_TIMEOUT_MS = 2000;
const DISCONNECT_TIMEOUT_MS = 2000;

class PostMessageChannel implements Channel {
  #origin: string;
  #source: WindowProxy;
  #closed = false;
  #requests: JsonRequest[] = [];
  #requestListeners = new Set<(request: JsonRequest) => void>();
  #closeListeners = new Set<() => void>();

  constructor(origin: string, source: WindowProxy) {
    this.#origin = origin;
    this.#source = source;

    window.addEventListener("message", (event) => {
      const { data, success } = JsonRequestSchema.safeParse(event.data);
      console.log("Received message", event.data);
      if (
        event.source !== this.#source ||
        event.origin !== this.#origin ||
        !success ||
        data?.method === "icrc29_status"
      ) {
        return;
      }
      this.#requests.push(data);
      console.log("requests", this.#requests);
      this.#requestListeners.forEach((listener) => listener(data));
    });
  }

  get origin() {
    return this.#origin;
  }

  get closed() {
    return this.#closed;
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
    this.#closed = true;
    this.#closeListeners.forEach((listener) => listener());
    return Promise.resolve();
  }
}

export class PostMessageTransport implements Transport {
  establishChannel(options: ChannelOptions): Promise<PostMessageChannel> {
    return new Promise((resolve, reject) => {
      console.log("establishing");
      let channel: PostMessageChannel;
      new HeartbeatServer({
        status: options?.pending === true ? "pending" : "ready",
        allowedOrigin: options?.allowedOrigin,
        establishTimeout: ESTABLISH_TIMEOUT_MS,
        disconnectTimeout: DISCONNECT_TIMEOUT_MS,
        onEstablish: (origin, source) => {
          console.log("established!");
          channel = new PostMessageChannel(origin, source);
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
