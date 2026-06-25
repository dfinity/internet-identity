import { get, type Readable } from "svelte/store";
import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { writableStored } from "./writable.store";

/**
 * The single MCP server URL a user trusts on this device, per identity.
 *
 * Each user declares the one MCP server they trust (e.g. one they run
 * themselves) rather than every user trusting a single operator-configured
 * server. The `/mcp` connect flow delivers a standing delegation only when the
 * request's callback origin matches the trusted server's origin for the chosen
 * identity; otherwise it points the user at Settings to set it. The full URL is
 * stored (so the Settings UI can probe a path-based endpoint like
 * `https://host/mcp`), while trust matching is by origin.
 *
 * Browser-local and same-origin protected: only the user can set it (via
 * Settings), never a page that initiated the connect request. The authoritative
 * trust is still the backend binding `mcp_set_access` creates at connect time;
 * this is the user-controlled pre-gate in front of it.
 */
type McpTrustedServerState = {
  [identityNumber: string]: string;
};

type McpTrustedServerStore = Readable<McpTrustedServerState> & {
  /** Whether the identity's trusted server has the given origin. */
  isTrusted: (identityNumber: bigint, origin: string) => boolean;
  /** Set (replacing any previous) the trusted server URL for an identity. */
  set: (identityNumber: bigint, url: string) => void;
  /** Forget the trusted server for an identity. */
  clear: (identityNumber: bigint) => void;
};

const originOf = (url: string): string | undefined => {
  try {
    return new URL(url).origin;
  } catch {
    return undefined;
  }
};

export const initMcpTrustedServerStore = (): McpTrustedServerStore => {
  // version 2: the value shape changed from a list of origins (`string[]`) to a
  // single URL (`string`); the bump discards any incompatible persisted value.
  const store = writableStored<McpTrustedServerState>({
    key: storeLocalStorageKey.McpTrustedServers,
    defaultValue: {},
    version: 2,
  });

  return {
    subscribe: store.subscribe,
    isTrusted: (identityNumber, origin) => {
      const trusted = get(store)[identityNumber.toString()];
      return trusted !== undefined && originOf(trusted) === origin;
    },
    set: (identityNumber, url) => {
      store.update((state) => ({
        ...state,
        [identityNumber.toString()]: url,
      }));
    },
    clear: (identityNumber) => {
      store.update((state) => {
        const next = { ...state };
        delete next[identityNumber.toString()];
        return next;
      });
    },
  };
};

export const mcpTrustedServersStore = initMcpTrustedServerStore();
