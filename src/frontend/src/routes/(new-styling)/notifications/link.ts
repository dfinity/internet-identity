/**
 * What the installed app does on first launch: claim an entry with the token it was
 * installed with, then register for push under it.
 */
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import { ensureRegisteredDevice } from "$lib/utils/notifications/subscribeDevice";
import {
  appKeyActor,
  appPublicKey,
  ensureAppKey,
  readAppKey,
  rememberLinked,
} from "$lib/utils/notifications/notificationAppKey";
import type { LinkNotificationAppError } from "$lib/generated/internet_identity_types";
import type { LinkToken } from "./linkToken";

export type LinkOutcome =
  | { status: "linked"; identityNumber: bigint }
  /** The token was refused for any other reason, which a fresh one fixes. Starting the
   *  install again issues one, and that is a step the user has already seen. */
  | { status: "token-refused"; reason: string }
  | { status: "failed"; reason: string };

/**
 * What to show for a failure, which for a canister refusal is the variant it carries.
 *
 * `CanisterError` is constructed with no message, so reading `.message` off one yields
 * an empty string: the screen then named no reason at all and there was nothing to act
 * on. The variant is the whole of what the canister said.
 */
const describe = (error: unknown): string => {
  if (isCanisterError<LinkNotificationAppError>(error)) {
    return String(error.type);
  }
  return error instanceof Error ? error.message : String(error);
};

/** The identity this app is already linked to, where a previous launch linked it. */
export const linkedIdentity = (): Promise<bigint | undefined> =>
  readAppKey().then((record) => record?.identityNumber);

/**
 * Claims an entry with `token` and registers this app's push subscription under it.
 *
 * Idempotent in the way that matters: a launch that already linked skips the claim and
 * re-registers, because the subscription is the browser's and may have been replaced
 * since.
 */
export const linkAndRegister = async (
  token: LinkToken,
): Promise<LinkOutcome> => {
  try {
    const record = await ensureAppKey();
    const actor = await appKeyActor(record);

    if (record.identityNumber === undefined) {
      await actor
        .link_notification_app({
          anchor_number: token.identityNumber,
          app_key: await appPublicKey(record),
          expires_at_ns: token.expiresAtNs,
          signature: token.signature,
        })
        .then(throwCanisterError);
      await rememberLinked(record, token.identityNumber);
    }

    await ensureRegisteredDevice(token.identityNumber, actor);
    return { status: "linked", identityNumber: token.identityNumber };
  } catch (error) {
    const reason = describe(error);
    return isCanisterError(error)
      ? { status: "token-refused", reason }
      : { status: "failed", reason };
  }
};
