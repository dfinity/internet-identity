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
import { importLinkKey } from "$lib/utils/notifications/linkKey";
import { Principal } from "@icp-sdk/core/principal";
import { appKeyActor } from "./appKeyActor";

/** Must match `NOTIFICATION_APP_LINK_SIGNATURE_DOMAIN` in `browser_key.rs`. */
const LINK_SIGNATURE_DOMAIN = new TextEncoder().encode(
  "ii-notification-app-link",
);

/** Must match `link_message` in `browser_key.rs`: each principal behind its length, an
 *  absent installer as length zero. */
const linkMessage = (
  identityNumber: bigint,
  app: Uint8Array,
  installer: Uint8Array,
): Uint8Array<ArrayBuffer> => {
  const domain = LINK_SIGNATURE_DOMAIN;
  const message = new Uint8Array(
    domain.length + 8 + 1 + app.length + 1 + installer.length,
  );
  message.set(domain);
  new DataView(message.buffer, domain.length).setBigUint64(
    0,
    identityNumber,
    false,
  );
  let offset = domain.length + 8;
  message[offset] = app.length;
  message.set(app, offset + 1);
  offset += 1 + app.length;
  message[offset] = installer.length;
  message.set(installer, offset + 1);
  return message;
};

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
    // A signature that does not match is a damaged or tampered URL, which a fresh one fixes.
    case "NoPendingLink":
    case "InvalidLinkSignature":
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

/** Made as the app itself, carrying the signature of the key the install URL carried over
 *  this app and the installer the URL names: the canister takes it only with both. */
export const linkWithToken = async (
  record: AppKeyRecord,
  token: LinkToken,
): Promise<LinkOutcome> => {
  let request;
  try {
    const linkKey = await importLinkKey(token.linkKey);
    const app = await appPrincipal(record);
    const installer =
      token.installerKey === undefined
        ? undefined
        : Principal.selfAuthenticating(token.installerKey);
    const signature = await crypto.subtle.sign(
      { name: "ECDSA", hash: "SHA-256" },
      linkKey.privateKey,
      linkMessage(
        token.identityNumber,
        app.toUint8Array(),
        installer?.toUint8Array() ?? new Uint8Array(),
      ),
    );
    request = {
      anchor_number: token.identityNumber,
      link_key: new Uint8Array(
        await crypto.subtle.exportKey("spki", linkKey.publicKey),
      ),
      link_signature: new Uint8Array(signature),
      installer_principal: installer === undefined ? [] : [installer],
      app_id: record.appId === undefined ? [] : [record.appId],
    } satisfies Parameters<
      Awaited<ReturnType<typeof appKeyActor>>["link_notification_app"]
    >[0];
  } catch (error) {
    return { status: "token-refused", reason: describe(error) };
  }
  try {
    const linked = await (
      await appKeyActor(record)
    )
      .link_notification_app(request)
      .then(throwCanisterError);
    await rememberAppId(linked.app_id);
    return { status: "linked", appId: linked.app_id };
  } catch (error) {
    const reason = describe(error);
    return isCanisterError<LinkNotificationAppError>(error)
      ? refusal(error, reason)
      : { status: "failed", reason };
  }
};
