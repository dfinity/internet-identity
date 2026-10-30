/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** The authorize URL of the Internet Identity to sign in with. */
  readonly VITE_II_URL?: string;
  /** The canister of that Internet Identity. */
  readonly VITE_II_CANISTER_ID?: string;
  /** This app's canister, where the page's own host does not name it. */
  readonly VITE_CANISTER_ID?: string;
  /** "1" or "0". Unset, the page decides from its host. */
  readonly VITE_FETCH_ROOT_KEY?: string;
}
