// How notifications last went, so the next screen can be specific about which failure
// happened. Kept in localStorage and never sent anywhere.

const STORE_KEY = "ii-notification-diagnostics";

export type FailureReason =
  "permission-denied" | "subscribe-failed" | "register-failed" | "unsupported";

export interface NotificationDiagnostics {
  lastFailure?: { reason: FailureReason; message?: string; at: number };
  permission?: NotificationPermission;
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
  write({ ...read(), lastFailure: { reason, message, at: Date.now() } });
};

export const clearFailure = (): void => {
  const current = read();
  delete current.lastFailure;
  write(current);
};

export const recordPermission = (permission: NotificationPermission): void => {
  write({ ...read(), permission });
};
