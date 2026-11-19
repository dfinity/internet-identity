/** Must match `id`, `start_url` and `scope` in `static/ios-icp-alerts.webmanifest`.
 * A module of its own: the worker needs the path, and `installUrl.ts` reaches the document to mint a URL. */
export const NOTIFICATION_APP_PATH = "/ios-icp-alerts";

/** Where an app's installed web app on iOS opens Safari to link its entry to Safari's, with the one-shot link key in the fragment. */
export const SAME_DEVICE_LINK_PATH = "/ios-installed-app";
