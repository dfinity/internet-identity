/**
 * What a shown notification carries, so the worker can find it again.
 *
 * The tag names the notification, which is what makes the real content replace the
 * placeholder shown before it and a repeat of the same notification replace itself
 * rather than pile up. It names the identity as well: a notification id is scoped to
 * one app and one recipient, so two identities signed in on one browser can be handed
 * the same one. The data carries what the canister calls need, since a worker
 * woken later has nothing else to go on. Both are plain strings: notification data
 * goes through structured cloning, and `bigint` support for it is not everywhere.
 */

export interface NotificationRef {
  identityNumber: bigint;
  origin: string;
  /** `undefined` is the unreserved default account. */
  accountNumber?: bigint;
  canisterId: string;
  id: bigint;
}

export interface ShownData {
  identityNumber: string;
  origin: string;
  accountNumber: string;
  canisterId: string;
  id: string;
  /** Where a click goes, already checked against the app's own origins. */
  url: string;
}

export const tagOf = (ref: NotificationRef): string =>
  `${ref.identityNumber}|${ref.origin}|${ref.accountNumber ?? ""}|${ref.id}`;

export const dataOf = (ref: NotificationRef, url: string): ShownData => ({
  identityNumber: ref.identityNumber.toString(),
  origin: ref.origin,
  accountNumber: ref.accountNumber?.toString() ?? "",
  canisterId: ref.canisterId,
  id: ref.id.toString(),
  url,
});

/** The notification some data belongs to, or `undefined` for anything this worker did
 *  not write — an older version's notification, or a browser that dropped the data. */
export const refOf = (data: unknown): NotificationRef | undefined => {
  if (typeof data !== "object" || data === null) {
    return undefined;
  }
  const { identityNumber, origin, accountNumber, canisterId, id } =
    data as Partial<ShownData>;
  if (
    typeof identityNumber !== "string" ||
    typeof origin !== "string" ||
    typeof accountNumber !== "string" ||
    typeof canisterId !== "string" ||
    typeof id !== "string"
  ) {
    return undefined;
  }
  try {
    return {
      identityNumber: BigInt(identityNumber),
      origin,
      accountNumber: accountNumber === "" ? undefined : BigInt(accountNumber),
      canisterId,
      id: BigInt(id),
    };
  } catch {
    return undefined;
  }
};
