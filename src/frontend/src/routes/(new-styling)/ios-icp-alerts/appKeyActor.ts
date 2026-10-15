/**
 * The app's actor as the page builds it, from the globals the document carries.
 *
 * Separate from the worker's, which has no document to read them from.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { agentOptions, canisterId } from "$lib/globals";
import { appKeyActorWith } from "$lib/utils/notifications/notificationAppActor";
import type { AppKeyRecord } from "$lib/utils/notifications/notificationAppKey";

export const appKeyActor = (
  record: AppKeyRecord,
): Promise<ActorSubclass<_SERVICE>> =>
  appKeyActorWith(record, { agentOptions, canisterId });
