/**
 * The identities the landing playground shows, for each state it compares.
 * The first three are the concept's own (Thomas, Work, Testing, from
 * `../mock/mockData.ts`); "Two" is Thomas and Work, a passkey and an SSO
 * account; "Many" adds four more, to see where a layout runs out of room. Nothing here talks to a canister.
 */
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { identities as conceptIdentities } from "../mock/mockData";

const HOUR = 60 * 60 * 1000;
const DAY = 24 * HOUR;
const now = Date.now();

export type StateId = "none" | "one" | "two" | "several" | "many";

const passkey = (
  identityNumber: number,
  name: string,
  agoMillis: number,
): LastUsedIdentity => ({
  identityNumber: BigInt(identityNumber),
  name,
  authMethod: { passkey: { credentialId: new Uint8Array([identityNumber]) } },
  lastUsedTimestampMillis: now - agoMillis,
});

const concept = conceptIdentities.map(({ identity }) => identity);

const more: LastUsedIdentity[] = [
  passkey(10_211, "Family", 9 * DAY),
  passkey(10_342, "Investments", 21 * DAY),
  passkey(10_377, "Hackathon", 48 * DAY),
  passkey(10_420, "Old laptop", 120 * DAY),
];

/** The identities on this device, in each state; the most recent first. */
export const IDENTITIES_BY_STATE: Record<StateId, LastUsedIdentity[]> = {
  none: [],
  one: concept.slice(0, 1),
  two: concept.slice(0, 2),
  several: concept,
  many: [...concept, ...more],
};

/** What an identity signs in with, as the product's switcher says it. */
export const methodOf = ({ authMethod }: LastUsedIdentity): string => {
  if ("sso" in authMethod) {
    const { sso } = authMethod;
    return sso.email ?? sso.name ?? sso.domain;
  }
  if ("openid" in authMethod) {
    return "Google";
  }
  return "Passkey";
};

export const nameOf = (one: LastUsedIdentity): string =>
  one.name ?? `${one.identityNumber}`;

/** "3 hours ago", for where a layout says when an identity was last used. */
export const agoOf = ({
  lastUsedTimestampMillis,
}: LastUsedIdentity): string => {
  const minutes = Math.round((now - lastUsedTimestampMillis) / 60_000);
  if (minutes < 2) {
    return "just now";
  }
  if (minutes < 60) {
    return `${minutes} minutes ago`;
  }
  const hours = Math.round(minutes / 60);
  if (hours < 24) {
    return hours === 1 ? "an hour ago" : `${hours} hours ago`;
  }
  const days = Math.round(hours / 24);
  return days === 1 ? "yesterday" : `${days} days ago`;
};

/** "Good afternoon", as the concept's greeting says it. */
export const greeting = (): string => {
  const hour = new Date().getHours();
  return hour < 5
    ? "Good evening"
    : hour < 12
      ? "Good morning"
      : hour < 18
        ? "Good afternoon"
        : "Good evening";
};
