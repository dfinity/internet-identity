import { Actor, HttpAgent, SignIdentity } from "@dfinity/agent";

export const identityFromActor = async (actor: Actor) => {
  const agent = Actor.agentOf(actor);
  if (!agent || !(agent instanceof HttpAgent)) {
    throw new Error("HttpAgent not found in actor");
  }
  const identity = await agent.config.identity;
  if (!(identity instanceof SignIdentity)) {
    throw new Error("Identity is not a SignIdentity");
  }
  return identity;
};
