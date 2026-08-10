// Browser-local, per-anchor persistence for the multiple-accounts toggle.
// Per-anchor (not per-dapp) because the toggle is a mental-mode switch:
// a user who self-identifies as a multi-accounts user wants the
// affordance everywhere, not separately for each dapp.
const TOGGLE_STORAGE_PREFIX = "ii:multi-accounts:";

const toggleKey = (anchor: bigint): string =>
  `${TOGGLE_STORAGE_PREFIX}${anchor.toString()}`;

/** Whether this anchor has opted into picking an account per sign-in. */
export const readMultipleAccountsToggle = (anchor: bigint): boolean => {
  if (typeof localStorage === "undefined") return false;
  return localStorage.getItem(toggleKey(anchor)) === "1";
};

export const writeMultipleAccountsToggle = (
  anchor: bigint,
  enabled: boolean,
): void => {
  if (typeof localStorage === "undefined") return;
  const key = toggleKey(anchor);
  if (enabled) {
    localStorage.setItem(key, "1");
  } else {
    localStorage.removeItem(key);
  }
};
