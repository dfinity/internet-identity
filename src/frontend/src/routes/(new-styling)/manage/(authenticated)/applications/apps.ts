import type { ApplicationInfo } from "$lib/generated/internet_identity_types";
import { nanosToMillis } from "$lib/utils/time";

export interface App {
  /** The origin the app's identity is derived for. Its metadata and its
   *  notification consent are keyed by the same spelling. */
  origin: string;
  /** The latest sign-in to any of the identity's accounts at the app. */
  lastUsedMillis: number;
  /** Whether the identity allows the app to notify it, as the canister holds it. */
  notificationsAllowed: boolean;
}

/** The identity's apps as the canister lists them, most recently used first. */
export const appsFrom = (applications: ApplicationInfo[]): App[] =>
  applications
    .map(({ origin, last_used, notifications_allowed }) => ({
      origin,
      lastUsedMillis: nanosToMillis(last_used),
      notificationsAllowed: notifications_allowed,
    }))
    .sort((a, b) => {
      const byTime = b.lastUsedMillis - a.lastUsedMillis;
      return byTime !== 0 ? byTime : a.origin.localeCompare(b.origin);
    });
