import { derived, get, type Readable } from "svelte/store";
import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { writableStored } from "./writable.store";

type McpAccessState = {
  [identityNumber: string]: boolean;
};

type McpAccessStore = Readable<McpAccessState> & {
  isEnabled: (identityNumber: bigint) => boolean;
  enable: (identityNumber: bigint) => void;
  disable: (identityNumber: bigint) => void;
};

export const initMcpAccessStore = (): McpAccessStore => {
  const store = writableStored<McpAccessState>({
    key: storeLocalStorageKey.McpAccess,
    defaultValue: {},
    version: 1,
  });

  return {
    subscribe: store.subscribe,
    isEnabled: (identityNumber) =>
      get(store)[identityNumber.toString()] === true,
    enable: (identityNumber) => {
      store.update((state) => ({
        ...state,
        [identityNumber.toString()]: true,
      }));
    },
    disable: (identityNumber) => {
      store.update((state) => {
        const next = { ...state };
        delete next[identityNumber.toString()];
        return next;
      });
    },
  };
};

export const mcpAccessStore = initMcpAccessStore();

/** Reactive boolean for a specific identity. */
export const isMcpAccessEnabledStore = (
  identityNumber: bigint,
): Readable<boolean> =>
  derived(mcpAccessStore, (state) => state[identityNumber.toString()] === true);
