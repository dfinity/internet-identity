import { writable } from "svelte/store";

/**
 * Whether the identity switcher in the /mcp layout header should be shown.
 *
 * The page sets this to `true` only during the sign-in phases (wizard,
 * authorize) and `false` on the terminal screens (close, error, invalid,
 * mcp-disabled), where switching identity is meaningless. It lives in a shared
 * store because the switcher is rendered by the layout but its relevance is
 * owned by the page's phase.
 */
export const showIdentitySwitcher = writable(false);
