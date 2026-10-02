/**
 * What this app is called, in one place.
 *
 * iOS shows it on the Home Screen and in Settings, and the install and unblock steps
 * both tell the user to look for it by name, so the mocks and the manifest have to
 * agree. They did not: the manifest's `short_name` said one thing and the step mocks
 * another, which sent the user hunting for an app that was not there under that name.
 *
 * Must match `name` in `static/notifications.webmanifest`.
 */
export const NOTIFICATION_APP_NAME = "Internet Identity Notifications";
