/**
 * What the app does on the launch it was installed by: spend the token it carries.
 *
 * The token is the installing browser's signature, and spending it is what gives this
 * app a record of its own and points that browser at it. Nothing else can: the app runs
 * in a storage partition no browser can reach, and the key it signs with is one the
 * canister has never seen before this call.
 */
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import {
  appPublicKey,
  type AppKeyRecord,
} from "$lib/utils/notifications/notificationAppKey";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  LinkNotificationAppError,
} from "$lib/generated/internet_identity_types";
import type { LinkToken } from "./linkToken";

export type LinkOutcome =
  | { status: "linked" }
  /** The canister refused the token itself, which a fresh one fixes: the user starts the
   *  install again, which is a step they have already seen. */
  | { status: "token-refused"; reason: string }
  | { status: "failed"; reason: string };

/**
 * What to show for a failure, which for a canister refusal is the variant it carries.
 *
 * `CanisterError` is constructed with no message, so reading `.message` off one yields an
 * empty string and the screen would name no reason at all.
 */
const describe = (error: unknown): string => {
  if (isCanisterError<LinkNotificationAppError>(error)) {
    return String(error.type);
  }
  return error instanceof Error ? error.message : String(error);
};

/** Whether the canister refused the token itself, which is the only failure a fresh one
 *  answers. */
const refusedTheToken = (error: unknown): boolean =>
  isCanisterError<LinkNotificationAppError>(error) &&
  (error.type === "InvalidLinkToken" || error.type === "LinkTokenExpired");

export const linkWithToken = async (
  actor: ActorSubclass<_SERVICE>,
  record: AppKeyRecord,
  token: LinkToken,
): Promise<LinkOutcome> => {
  try {
    await actor
      .link_notification_app({
        anchor_number: token.identityNumber,
        app_key: await appPublicKey(record),
        expires_at_ns: token.expiresAtNs,
        signature: token.signature,
      })
      .then(throwCanisterError);
    return { status: "linked" };
  } catch (error) {
    const reason = describe(error);
    return refusedTheToken(error)
      ? { status: "token-refused", reason }
      : { status: "failed", reason };
  }
};
