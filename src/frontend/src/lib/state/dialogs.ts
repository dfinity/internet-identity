import { get, writable } from "svelte/store";

export enum DialogId {
  Test = "test",
  Test2 = "test2",
}

export type DialogData = Record<string, unknown>;

type DialogState = {
  isOpen: boolean;
  data?: DialogData;
};

export interface DialogDataMap {
  [DialogId.Test]: { test: string };
  [DialogId.Test2]: { callback: () => void };
}

const createDialogsStore = () => {
  const { subscribe, update, set } = writable<Record<DialogId, DialogState>>(
    {} as Record<DialogId, DialogState>,
  );

  const openDialog = <T extends DialogId>(
    dialogId: T,
    data?: DialogDataMap[T],
  ) => {
    update((dialogs) => {
      const closedDialogs = Object.keys(dialogs).reduce(
        (acc, key) => ({
          ...acc,
          [key as DialogId]: { isOpen: false },
        }),
        {} as Record<DialogId, DialogState>,
      );

      return {
        ...closedDialogs,
        [dialogId]: {
          isOpen: true,
          data,
        },
      };
    });
  };

  const closeDialog = (dialogId: DialogId) => {
    update((dialogs) => ({
      ...dialogs,
      [dialogId]: { isOpen: false },
    }));
  };

  const getDialogData = <T extends DialogId>(dialogId: T): DialogDataMap[T] => {
    if (typeof window === "undefined") return {} as DialogDataMap[T];
    const dialogs = get({ subscribe });
    return (
      (dialogs[dialogId]?.data as DialogDataMap[T]) ?? ({} as DialogDataMap[T])
    );
  };

  const resetDialogs = () => {
    set({} as Record<DialogId, DialogState>);
  };

  return {
    subscribe,
    openDialog,
    closeDialog,
    getDialogData,
    resetDialogs,
  };
};

export const dialogsStore = createDialogsStore();
