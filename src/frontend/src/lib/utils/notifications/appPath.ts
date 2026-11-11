/** Must match `id`, `start_url` and `scope` in `static/ios-icp-alerts.webmanifest`.
 * A module of its own: the worker needs the path, and `installUrl.ts` reaches the document to mint a URL. */
export const NOTIFICATION_APP_PATH = "/ios-icp-alerts";
