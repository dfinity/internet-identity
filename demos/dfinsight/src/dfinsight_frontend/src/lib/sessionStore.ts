import type { Session } from "./auth";

// Tiny in-memory session holder. The auth client persists the
// delegation in IndexedDB; this module just holds the actor + identity
// for the lifetime of the page so we don't reconstruct them on every
// route change. AdminSessions are deliberately not restored across
// reloads — re-auth is required to refresh the attribute bundle.
let current: Session | null = null;
const listeners = new Set<(s: Session | null) => void>();

export const sessionStore = {
  get(): Session | null {
    return current;
  },
  set(s: Session | null) {
    current = s;
    for (const l of listeners) l(s);
  },
  subscribe(l: (s: Session | null) => void): () => void {
    listeners.add(l);
    return () => listeners.delete(l);
  },
};
