/**
 * Rendered at build time, unlike the rest of the app.
 *
 * iOS reads what it needs for Add to Home Screen — the icon, the name, the manifest —
 * out of the HTML it fetches, not out of the live DOM. With `ssr` off, as it is
 * everywhere else here, `<svelte:head>` is injected by the client and that HTML carries
 * none of it: the installed app came out with a letter placeholder for an icon and a
 * name iOS had made up for itself.
 *
 * `prerender` is inherited. Nothing on this page may touch `window` while rendering.
 */
export const ssr = true;
