/**
 * What a woken worker cannot work out for itself, and how it travels.
 *
 * The page writes it onto the worker's registration script URL and the registration
 * keeps that URL, so a worker woken long afterwards reads it back out of its own
 * `location.search`. Nothing else crosses: there is no page to ask and no document to
 * read.
 */

import type { HttpAgentOptions } from "@icp-sdk/core/agent";
import { z } from "zod";

/** The agent options the page resolves and a worker cannot. Taken from the agent's own
 *  option type, so the two cannot drift. */
export type AgentOptions = Required<
  Pick<HttpAgentOptions, "host" | "shouldFetchRootKey">
>;

const agentOptions = z.object({
  host: z.string(),
  shouldFetchRootKey: z.boolean(),
}) satisfies z.ZodType<AgentOptions>;

/** Which canister to ask, and how to reach it. */
export interface WorkerRegistration {
  canisterId: string;
  agentOptions: AgentOptions;
}

export const registrationSearch = ({
  canisterId,
  agentOptions,
}: WorkerRegistration): string =>
  new URLSearchParams({
    canisterId,
    agentOptions: JSON.stringify(agentOptions),
  }).toString();

/**
 * What the page put on the URL, or nothing where it cannot be read.
 *
 * Validated rather than trusted: `shouldFetchRootKey` decides whether a local
 * replica's answers are accepted, so it counts only as the boolean the page wrote.
 */
export const registrationFrom = (
  search: string,
): WorkerRegistration | undefined => {
  const params = new URLSearchParams(search);
  const canisterId = params.get("canisterId");
  const encoded = params.get("agentOptions");
  if (canisterId === null || encoded === null) {
    return undefined;
  }
  try {
    const parsed = agentOptions.safeParse(JSON.parse(encoded));
    return parsed.success
      ? { canisterId, agentOptions: parsed.data }
      : undefined;
  } catch {
    // Not JSON at all.
    return undefined;
  }
};
