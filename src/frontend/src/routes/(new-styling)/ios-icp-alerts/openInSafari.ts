/**
 * Leaves the Home Screen app for Safari.
 *
 * An ordinary link to another path opens inside the app's own view, which has storage of
 * its own and no sign-in, so the user arrives signed out. `x-safari-https:` hands the URL
 * to Safari instead, but only when a page navigates itself there: given to
 * `clients.openWindow` it opens the web app and loses the link.
 */
export const openInSafari = (url: string): void => {
  window.location.href = url.replace(/^https:/, "x-safari-https:");
};
