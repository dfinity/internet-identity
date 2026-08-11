import { createStore, set as idbSet, del as idbDel } from "idb-keyval";
import type { PushDelegationRecord } from "$lib/utils/authentication/pushDelegation";

// A dedicated IndexedDB database, separate from the session-delegation one, so
// II's service worker can open it by name and read a per-origin delegation
// directly — without importing idb-keyval or the app bundle. The names are the
// wire contract with the worker: it opens `PUSH_DELEGATION_DB` and reads
// `PUSH_DELEGATION_STORE_NAME` keyed by origin. Keep them in sync with
// service-worker.js.
export const PUSH_DELEGATION_DB = "ii-push-delegations";
export const PUSH_DELEGATION_STORE_NAME = "byOrigin";

const STORE = createStore(PUSH_DELEGATION_DB, PUSH_DELEGATION_STORE_NAME);

export const storePushDelegation = async (
  record: PushDelegationRecord,
): Promise<void> => {
  await idbSet(record.origin, record, STORE);
};

export const purgePushDelegation = async (origin: string): Promise<void> => {
  await idbDel(origin, STORE);
};
