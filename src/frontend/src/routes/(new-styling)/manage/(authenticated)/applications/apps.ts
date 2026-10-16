import type { ActorSubclass } from "@icp-sdk/core/agent";
import { derived, get, type Readable } from "svelte/store";
import type {
  _SERVICE,
  ApplicationInfo,
} from "$lib/generated/internet_identity_types";
import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
import { nanosToMillis } from "$lib/utils/time";
import { GATEWAY_ORIGIN_REGEX, originLabel } from "$lib/utils/urlUtils";
import { throwCanisterError } from "$lib/utils/utils";

export interface App {
  /** The origin the app's identity is derived for. Its metadata and its
   *  notification consent are keyed by the same spelling. */
  origin: string;
  /** The latest sign-in to any of the identity's accounts at the app, or `undefined`
   *  where none was used. */
  lastUsedMillis: number | undefined;
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

/** The identity's apps as the canister lists them, most recently used first and the
 *  ones never used last. */
export const appsFrom = (applications: ApplicationInfo[]): App[] =>
  applications
    .map(({ origin, last_used, notifications_allowed, last_notified }) => ({
      origin,
      lastUsedMillis:
        last_used[0] !== undefined ? nanosToMillis(last_used[0]) : undefined,
      notificationsAllowed: notifications_allowed,
      lastNotifiedMillis:
        last_notified[0] !== undefined
          ? nanosToMillis(last_notified[0])
          : undefined,
    }))
    .sort((a, b) => {
      const byTime = (b.lastUsedMillis ?? -1) - (a.lastUsedMillis ?? -1);
      return byTime !== 0 ? byTime : a.origin.localeCompare(b.origin);
    });

/** The app's published name, or where it is shown when it publishes none. Follows the
 *  metadata as it resolves. */
export const appNameStore = (origin: string): Readable<string> =>
  derived(
    getAppMetadataStore(origin),
    (metadata) => metadata.name ?? originLabel(appUrl(origin)),
  );

/** {@link appNameStore} as it reads now. */
export const appName = (origin: string): string => get(appNameStore(origin));

/** Undoes every permission the identity decided for the apps and signs it out of them,
 *  in one call. */
export const forgetApps = ({
  identityNumber,
  origins,
  actor,
}: {
  identityNumber: bigint;
  origins: string[];
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> =>
  actor
    .forget_applications({ anchor_number: identityNumber, origins })
    .then(throwCanisterError)
    .then(() => undefined);
