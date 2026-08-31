import { get, type Readable } from "svelte/store";
import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { writableStored } from "$lib/stores/writable.store";

/**
 * Which identities have already signed in to a local MCP server on *this*
 * computer, so the notice that precedes the first such sign-in is shown once
 * per identity per machine.
 *
 * This is a record, not a permission. What a local server may do is decided by
 * the identity's synced config (which the user can revoke from any device,
 * taking the live session with it) and by the consent screen on every connect.
 * All this store decides is whether the user has already been told that the
 * connector they are signing in to is a program on their own computer.
 *
 * Device-local because the fact it records is device-local: the identity's
 * permission to use a local connector follows it everywhere, but "you have
 * already been shown this here" is only ever true of one machine.
 */
type LocalServerNoticeState = {
  [identityNumber: string]: boolean;
};

type LocalServerNoticeStore = Readable<LocalServerNoticeState> & {
  isAcknowledged: (identityNumber: bigint) => boolean;
  acknowledge: (identityNumber: bigint) => void;
};

export const initLocalServerNoticeStore = (): LocalServerNoticeStore => {
  const store = writableStored<LocalServerNoticeState>({
    key: storeLocalStorageKey.McpLocalServerNotice,
    defaultValue: {},
    version: 1,
  });

  return {
    subscribe: store.subscribe,
    isAcknowledged: (identityNumber) =>
      get(store)[identityNumber.toString()] === true,
    acknowledge: (identityNumber) => {
      store.update((state) => ({
        ...state,
        [identityNumber.toString()]: true,
      }));
    },
  };
};

export const localServerNoticeStore = initLocalServerNoticeStore();
