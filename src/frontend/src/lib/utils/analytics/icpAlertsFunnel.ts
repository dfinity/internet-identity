import { Funnel } from "./Funnel";

/**
 * Turning notifications on where they arrive in the ICP Alerts Home Screen app (iOS).
 *
 * Square brackets [] indicate optional events.
 *
 * The flow crosses three documents, and Plausible tells visitors apart by IP and user
 * agent rather than by anything stored, so only the first two are reliably one visitor:
 *
 * 1. The page that offers it (`source = authorize | settings`):
 *
 *    start-icp-alerts
 *      [icp-alerts--skipped]
 *      icp-alerts--enable-pressed
 *        icp-alerts--enabled           (an app on this device already delivers)
 *        | icp-alerts--install-opened  (with `tab = opened | refused`)
 *        | icp-alerts--failed          (with `reason`)
 *    end-icp-alerts
 *
 * 2. The Safari tab with the install steps:
 *
 *    icp-alerts--steps-shown
 *      [icp-alerts--said-already-added]
 *      [icp-alerts--app-detected]       (the app linked and registered while the tab was open)
 *
 * 3. The Home Screen app, whose user agent differs from Safari's:
 *
 *    icp-alerts--app-linked
 *      | icp-alerts--app-link-refused   (with `reason = stale-link | unusable-key`)
 *      icp-alerts--permission-asked
 *        icp-alerts--permission-granted
 *          icp-alerts--ready
 *        | icp-alerts--permission-denied
 *        | icp-alerts--permission-dismissed
 *
 * Every event in 2 and 3 carries `from`, the `source` that started the install. It
 * travels in the install URL's fragment, which iOS bookmarks whole, so the app can be
 * segmented by where its install was offered even where it counts as another visitor.
 */
export const IcpAlertsEvents = {
  EnablePressed: "enable-pressed",
  Enabled: "enabled",
  InstallOpened: "install-opened",
  Skipped: "skipped",
  Failed: "failed",
  StepsShown: "steps-shown",
  SaidAlreadyAdded: "said-already-added",
  AppDetected: "app-detected",
  AppLinked: "app-linked",
  AppLinkRefused: "app-link-refused",
  PermissionAsked: "permission-asked",
  PermissionGranted: "permission-granted",
  PermissionDenied: "permission-denied",
  PermissionDismissed: "permission-dismissed",
  Ready: "ready",
} as const;

export const icpAlertsFunnel = new Funnel<typeof IcpAlertsEvents>(
  "icp-alerts",
  true,
);

/** Where an install was offered. */
export type NotificationsSource = "authorize" | "settings";

const FROM_PARAM = "from";

/** Appended to the install URL's fragment next to the link token. */
export const installSourceParam = (source: NotificationsSource): string =>
  new URLSearchParams({ [FROM_PARAM]: source }).toString();

/** `undefined` where the fragment carries no source this build knows. */
export const installSourceOf = (
  hash: string,
): NotificationsSource | undefined => {
  const from = new URLSearchParams(hash.replace(/^#/, "")).get(FROM_PARAM);
  switch (from) {
    case "authorize":
    case "settings":
      return from;
    default:
      return undefined;
  }
};
