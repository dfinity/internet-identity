/**
 * Whether this document is the installed Home Screen app rather than a browser tab.
 *
 * No API reports "installed", only "running installed", which is the question this page
 * actually has: the same URL shows the install steps in a tab and runs the app on the
 * Home Screen. `display-mode` is the standard; `navigator.standalone` is what iOS
 * answered with before it supported one, and iOS is the only platform this matters on.
 */
export const isStandalone = (): boolean => {
  const legacy = (navigator as { standalone?: boolean }).standalone;
  return (
    legacy === true ||
    window.matchMedia("(display-mode: standalone)").matches ||
    window.matchMedia("(display-mode: fullscreen)").matches
  );
};
