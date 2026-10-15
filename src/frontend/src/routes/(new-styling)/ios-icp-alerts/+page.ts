/**
 * Rendered at build time, unlike the rest of the app.
 *
 * iOS reads what it needs for Add to Home Screen, the icon, the name and the manifest,
 * out of the HTML it fetches rather than out of the live DOM. With `ssr` off, as it is
 * everywhere else here, `<svelte:head>` is injected by the client and that HTML carries
 * none of it: the installed app comes out with a letter for an icon and a name iOS made
 * up for itself.
 *
 * `prerender` is inherited. Nothing on this page may touch `window` while rendering.
 */
export const ssr = true;
