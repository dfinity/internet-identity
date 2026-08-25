// A little local memory of how notifications last went, so the next screen can
// be specific instead of generic: which failure happened, which browser to give
// unblock steps for, and which apps the user recently declined. None of this is
// sensitive; it stays in localStorage and never leaves the browser.

const STORE_KEY = "ii-notification-diagnostics";
// How long a "Maybe later" quiets the opt-in for one app.
const DECLINE_COOLDOWN_MS = 14 * 24 * 60 * 60 * 1000;

export type FailureReason =
  | "permission-denied"
  | "subscribe-failed"
  | "register-failed"
  | "backend-disabled"
  | "unsupported";

export type BrowserKind =
  | "chrome"
  | "firefox"
  | "safari"
  | "edge"
  | "ios"
  | "android"
  | "other";

export interface NotificationDiagnostics {
  lastFailure?: { reason: FailureReason; message?: string; at: number };
  browser?: BrowserKind;
  permission?: NotificationPermission;
  declinedFor?: Record<string, number>;
}

const read = (): NotificationDiagnostics => {
  try {
    const raw = localStorage.getItem(STORE_KEY);
    return raw === null ? {} : (JSON.parse(raw) as NotificationDiagnostics);
  } catch {
    return {};
  }
};

const write = (value: NotificationDiagnostics): void => {
  try {
    localStorage.setItem(STORE_KEY, JSON.stringify(value));
  } catch {
    // A full or unavailable localStorage only costs us tailored copy, so ignore.
  }
};

export const readDiagnostics = (): NotificationDiagnostics => read();

export const recordFailure = (
  reason: FailureReason,
  message?: string,
): void => {
  write({
    ...read(),
    browser: detectBrowser(),
    lastFailure: { reason, message, at: Date.now() },
  });
};

export const clearFailure = (): void => {
  const current = read();
  delete current.lastFailure;
  write(current);
};

export const recordPermission = (permission: NotificationPermission): void => {
  write({ ...read(), permission });
};

export const recordDeclined = (origin: string): void => {
  const current = read();
  write({
    ...current,
    declinedFor: { ...current.declinedFor, [origin]: Date.now() },
  });
};

export const wasDeclinedRecently = (origin: string): boolean => {
  const at = read().declinedFor?.[origin];
  return at !== undefined && Date.now() - at < DECLINE_COOLDOWN_MS;
};

/** Best-effort engine sniff, only ever used to pick which unblock steps to show. */
export const detectBrowser = (): BrowserKind => {
  if (typeof navigator === "undefined") {
    return "other";
  }
  const ua = navigator.userAgent;
  if (/iPhone|iPad|iPod/.test(ua)) {
    return "ios";
  }
  if (/Android/.test(ua)) {
    return "android";
  }
  if (/Edg\//.test(ua)) {
    return "edge";
  }
  if (/Firefox\//.test(ua)) {
    return "firefox";
  }
  if (/Chrome\//.test(ua)) {
    return "chrome";
  }
  if (/Safari\//.test(ua)) {
    return "safari";
  }
  return "other";
};
