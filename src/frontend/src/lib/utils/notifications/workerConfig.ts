/**
 * What a woken worker cannot work out for itself.
 *
 * The page writes it onto the worker's registration script URL and the registration
 * keeps that URL, so a worker woken long afterwards reads it back out of its own
 * `location.search`. Nothing else crosses: there is no page to ask and no document to
 * read. Read once at startup and constant for the worker's life, so the worker's
 * modules import it the way the page's import `$lib/globals`.
 */

import type { HttpAgentOptions } from "@icp-sdk/core/agent";
import { z } from "zod";

/** The agent options the page resolves and a worker cannot. Taken from the agent's own
 *  option type, so the two cannot drift. */
export type AgentOptions = Required<
  Pick<HttpAgentOptions, "host" | "shouldFetchRootKey">
>;

/** Which Internet Identity to ask, and how to reach it. */
export interface WorkerConfig {
  canisterId: string;
  agentOptions: AgentOptions;
}

const schema = z.object({
  canisterId: z.string(),
  agentOptions: z.object({
    host: z.string(),
    shouldFetchRootKey: z.boolean(),
  }),
}) satisfies z.ZodType<WorkerConfig>;

/** SvelteKit bundles the worker here. */
const WORKER_PATH = "/service-worker.js";

/** Where to register the worker, carrying what it will need. */
export const workerConfigUrl = (config: WorkerConfig): string => {
  const url = new URL(WORKER_PATH, self.location.origin);
  url.searchParams.set("config", JSON.stringify(config));
  return url.toString();
};

/**
 * What the page put on the URL, or nothing where it cannot be read.
 */
export const decodeWorkerConfig = (
  search: string,
): WorkerConfig | undefined => {
  const encoded = new URLSearchParams(search).get("config");
  if (encoded === null) {
    return undefined;
  }
  try {
    const parsed = schema.safeParse(JSON.parse(encoded));
    return parsed.success ? parsed.data : undefined;
  } catch {
    // Not JSON at all.
    return undefined;
  }
};

export let config: WorkerConfig;

/** Reads the config off `search`. Answers whether it could: a worker that cannot has
 *  nothing to tell anyone and nothing to show but a placeholder. */
export const initWorkerConfig = (search: string): boolean => {
  const decoded = decodeWorkerConfig(search);
  if (decoded === undefined) {
    return false;
  }
  config = decoded;
  return true;
};
