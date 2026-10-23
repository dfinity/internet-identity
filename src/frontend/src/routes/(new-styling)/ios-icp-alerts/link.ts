import {
  type CanisterError,
  isCanisterError,
  throwCanisterError,
} from "$lib/utils/utils";
import {
  appPrincipal,
  rememberAppId,
  type AppKeyRecord,
} from "$lib/utils/notifications/notificationAppKey";
import type { LinkNotificationAppError } from "$lib/generated/internet_identity_types";
import type { LinkToken } from "$lib/utils/notifications/linkToken";
import { linkKeyActor } from "./appKeyActor";

export type LinkOutcome =
  /** Carries the record the canister allocated, since the stored one is a launch behind. */
  | { status: "linked"; appId: bigint }
  /** The install URL is spent or unusable, which a fresh one fixes. */
  | { status: "token-refused"; reason: string }
  /** The canister refused the principal this app signs as: a key is minted once per storage partition, so only a fresh install mints another. */
  | { status: "key-refused"; reason: string }
  | { status: "failed"; reason: string };

/** `CanisterError` is constructed with no message, so reading `.message` off one yields an empty string. */
const describe = (error: unknown): string => {
  if (isCanisterError<LinkNotificationAppError>(error)) {
    return String(error.type);
  }
  return error instanceof Error ? error.message : String(error);
};

/** Exhaustive and without a catch-all: a refusal landing in the wrong arm ends the launch on a screen whose only offer is to try the same call again. */
const refusal = (
  error: CanisterError<LinkNotificationAppError>,
  reason: string,
): LinkOutcome => {
  switch (error.type) {
    case "NoPendingLink":
      return { status: "token-refused", reason };
    // Both are this app's own principal being one the canister will not take, which only
    // a fresh install mints another of: one a browser of the identity already signs as,
    // and the anonymous one.
    case "PrincipalAlreadyInUse":
    case "AppPrincipalIsAnonymous":
      return { status: "key-refused", reason };
    case "InternalCanisterError":
      return { status: "failed", reason };
  }
};

/** Signed with the key the install URL carried, which is the only caller the canister takes this call from. */
export const linkWithToken = async (
  record: AppKeyRecord,
  token: LinkToken,
): Promise<LinkOutcome> => {
  let actor;
  try {
    actor = await linkKeyActor(token.linkKey);
  } catch (error) {
    return { status: "token-refused", reason: describe(error) };
  }
  try {
    const linked = await actor
      .link_notification_app({
        anchor_number: token.identityNumber,
        app_principal: await appPrincipal(record),
        app_id: record.appId === undefined ? [] : [record.appId],
      })
      .then(throwCanisterError);
    await rememberAppId(record, linked.app_id);
    return { status: "linked", appId: linked.app_id };
  } catch (error) {
    const reason = describe(error);
    return isCanisterError<LinkNotificationAppError>(error)
      ? refusal(error, reason)
      : { status: "failed", reason };
  }
};
