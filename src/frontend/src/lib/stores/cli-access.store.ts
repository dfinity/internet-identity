import { derived, get, type Readable } from "svelte/store";
import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { writableStored } from "./writable.store";

type CliAccessState = {
  [identityNumber: string]: boolean;
};

type CliAccessStore = Readable<CliAccessState> & {
  isEnabled: (identityNumber: bigint) => boolean;
  enable: (identityNumber: bigint) => void;
  disable: (identityNumber: bigint) => void;
};

export const initCliAccessStore = (): CliAccessStore => {
  const store = writableStored<CliAccessState>({
    key: storeLocalStorageKey.CliAccess,
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

export const cliAccessStore = initCliAccessStore();

/** Reactive boolean for a specific identity. */
export const isCliAccessEnabledStore = (
  identityNumber: bigint,
): Readable<boolean> =>
  derived(cliAccessStore, (state) => state[identityNumber.toString()] === true);
