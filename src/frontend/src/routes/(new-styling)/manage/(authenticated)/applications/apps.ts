import type { ApplicationInfo } from "$lib/generated/internet_identity_types";
import { nanosToMillis } from "$lib/utils/time";
import { GATEWAY_ORIGIN_REGEX } from "$lib/utils/urlUtils";

export interface App {
  /** The origin the app's identity is derived for. Its metadata and its
   *  notification consent are keyed by the same spelling. */
  origin: string;
  /** The latest sign-in to any of the identity's accounts at the app. */
  lastUsedMillis: number;
  /** Whether the identity allows the app to notify it, as the canister holds it. */
  notificationsAllowed: boolean;
  /** When the app last notified the identity, or `undefined` where it never has. */
  lastNotifiedMillis: number | undefined;
}

/** Where the app is shown and opened: a canister on any gateway domain is on
 *  `icp.net`, and any other origin is as it stands. */
export const appUrl = (origin: string): string => {
  const groups = origin.match(GATEWAY_ORIGIN_REGEX)?.groups;
  return groups === undefined ? origin : `https://${groups.subdomain}.icp.net`;
};

/** The identity's apps as the canister lists them, most recently used first. */
export const appsFrom = (applications: ApplicationInfo[]): App[] =>
  applications
    .map(({ origin, last_used, notifications_allowed, last_notified }) => ({
      origin,
      lastUsedMillis: nanosToMillis(last_used),
      notificationsAllowed: notifications_allowed,
      lastNotifiedMillis:
        last_notified[0] !== undefined
          ? nanosToMillis(last_notified[0])
          : undefined,
    }))
    .sort((a, b) => {
      const byTime = b.lastUsedMillis - a.lastUsedMillis;
      return byTime !== 0 ? byTime : a.origin.localeCompare(b.origin);
    });
