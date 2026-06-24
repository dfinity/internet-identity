import { get, type Readable } from "svelte/store";
import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { writableStored } from "./writable.store";

/**
 * Per-identity allowlist of MCP server origins the user trusts on this device.
 *
 * Each user declares which MCP server they trust (e.g. one they run themselves)
 * rather than every user trusting a single operator-configured server. The
 * `/mcp` connect flow delivers a standing delegation to the server only when the
 * request's callback origin is on the trusted list for the chosen identity;
 * otherwise it points the user at Settings to add it. Stored origins are
 * normalized (scheme + host[:port], no path) so they compare directly against a
 * callback's origin.
 *
 * Browser-local and same-origin protected: only the user can add entries (via
 * Settings), never a page that initiated the connect request. The authoritative
 * trust is still the backend binding `mcp_set_access` creates at connect time;
 * this list is the user-controlled pre-gate in front of it.
 */
type McpTrustedServersState = {
  [identityNumber: string]: string[];
};

type McpTrustedServersStore = Readable<McpTrustedServersState> & {
  isTrusted: (identityNumber: bigint, origin: string) => boolean;
  /** Adds a normalized origin; idempotent. */
  add: (identityNumber: bigint, origin: string) => void;
  remove: (identityNumber: bigint, origin: string) => void;
};

const originsFor = (
  state: McpTrustedServersState,
  identityNumber: bigint,
): string[] => state[identityNumber.toString()] ?? [];

export const initMcpTrustedServersStore = (): McpTrustedServersStore => {
  const store = writableStored<McpTrustedServersState>({
    key: storeLocalStorageKey.McpTrustedServers,
    defaultValue: {},
    version: 1,
  });

  return {
    subscribe: store.subscribe,
    isTrusted: (identityNumber, origin) =>
      originsFor(get(store), identityNumber).includes(origin),
    add: (identityNumber, origin) => {
      store.update((state) => {
        const key = identityNumber.toString();
        const current = state[key] ?? [];
        if (current.includes(origin)) {
          return state;
        }
        return { ...state, [key]: [...current, origin] };
      });
    },
    remove: (identityNumber, origin) => {
      store.update((state) => {
        const key = identityNumber.toString();
        const next = (state[key] ?? []).filter((o) => o !== origin);
        const updated = { ...state };
        if (next.length === 0) {
          delete updated[key];
        } else {
          updated[key] = next;
        }
        return updated;
      });
    },
  };
};

export const mcpTrustedServersStore = initMcpTrustedServersStore();
