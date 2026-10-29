/** iOS reads the icon, the name and the manifest out of the HTML it fetches rather than out of the live DOM, and with `ssr` off `<svelte:head>` is injected by the client.
 * `prerender` is inherited, so nothing on this page may touch `window` while rendering. */
export const ssr = true;
