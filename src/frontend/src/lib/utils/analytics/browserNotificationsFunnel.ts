import { Funnel } from "./Funnel";

/**
 * Turning notifications on where they arrive in the browser itself.
 *
 * Square brackets [] indicate optional events.
 *
 * start-browser-notifications       (with `source = authorize | settings`)
 *   [browser-notifications--skipped]
 *   browser-notifications--enable-pressed
 *     browser-notifications--enabled
 *     | browser-notifications--permission-denied
 *       [browser-notifications--unblocked]   (authorize only, after the user lifts the block)
 *     | browser-notifications--permission-dismissed
 *     | browser-notifications--failed      (with `reason`)
 * end-browser-notifications
 */
export const BrowserNotificationsEvents = {
  EnablePressed: "enable-pressed",
  Enabled: "enabled",
  PermissionDenied: "permission-denied",
  PermissionDismissed: "permission-dismissed",
  Unblocked: "unblocked",
  Skipped: "skipped",
  Failed: "failed",
} as const;

export const browserNotificationsFunnel = new Funnel<
  typeof BrowserNotificationsEvents
>("browser-notifications", true);
