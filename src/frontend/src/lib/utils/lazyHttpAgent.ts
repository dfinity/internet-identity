import { HttpAgent, HttpAgentOptions } from "@dfinity/agent";

/**
 * Alternative to HttpAgent.create(), where the async initialization calls
 * are not made on creation but lazily on update/query/readState.
 */
export class LazyHttpAgent extends HttpAgent {
  #initialized = Promise.resolve(false);
  #shouldFetchRootKey: boolean;

  constructor(options: HttpAgentOptions) {
    super(options);
    this.#shouldFetchRootKey = options.shouldFetchRootKey ?? false;
  }

  static createLazy(options: HttpAgentOptions) {
    return new LazyHttpAgent(options);
  }

  async #initialize(): Promise<void> {
    if (await this.#initialized) {
      return;
    }
    const initPromises: Promise<Uint8Array | void>[] = [this.syncTime()];
    if (
      this.host.toString() !== "https://icp-api.io" &&
      this.#shouldFetchRootKey
    ) {
      initPromises.push(
        this.fetchRootKey().then((buffer) => new Uint8Array(buffer)),
      );
    }
    this.#initialized = Promise.all(initPromises).then(() => true);
    await this.#initialized;
  }

  async call(
    ...args: Parameters<HttpAgent["call"]>
  ): ReturnType<HttpAgent["call"]> {
    await this.#initialize();
    return super.call(...args);
  }

  async query(
    ...args: Parameters<HttpAgent["query"]>
  ): ReturnType<HttpAgent["query"]> {
    await this.#initialize();
    return super.query(...args);
  }

  async readState(
    ...args: Parameters<HttpAgent["readState"]>
  ): ReturnType<HttpAgent["readState"]> {
    await this.#initialize();
    return super.readState(...args);
  }
}
