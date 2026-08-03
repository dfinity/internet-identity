/**
 * A page that requires sign-in — the landing target for a push notification.
 *
 * This is the guarded-route half of the notification tap-through: II delivers
 * the user here (via its `/notify` consent-gated redirect), and if they are not
 * signed in yet this page bounces them through the ICRC-167 redirect flow and
 * back, so the tap ends with them *authenticated* on the page the notification
 * was about.
 *
 * The redirect transport requires the flow to start during initial page load —
 * the redirect unloads the page, so it cannot hang off a click handler. Hence
 * the check and the bounce both run at module scope.
 */
import { AuthClient } from "@icp-sdk/auth/client";
import { encodeSnapshot, NEXT_KEY, type FormSnapshot } from "./redirectFlow";
import "./main.css";

const CALLBACK_PATH = "/callback";

const status = (text: string): void => {
  const el = document.getElementById("status");
  if (el !== null) {
    el.textContent = text;
  }
};

/** The identity provider to bounce through. Carried in the query so the
 *  notification link can point at a specific II (local replica vs. staging)
 *  without rebuilding the app; falls back to this origin's usual dev II. */
const identityProvider = (): string =>
  new URL(window.location.href).searchParams.get("iiUrl") ??
  "http://localhost:5173";

const run = async (): Promise<void> => {
  const authClient = new AuthClient({ idleOptions: { disableIdle: true } });

  if (await authClient.isAuthenticated()) {
    const identity = await authClient.getIdentity();
    status(`Signed in as ${identity.getPrincipal().toText()}`);
    return;
  }

  // Not signed in: hand off to the flow driver with `next` pointing back here,
  // including this page's own query so the II URL survives the round trip.
  const here = new URL(window.location.href);
  const snapshot: FormSnapshot = {
    iiUrl: identityProvider(),
    [NEXT_KEY]: `${here.pathname}${here.search}`,
  };
  status("Signing you in…");
  window.location.assign(`${CALLBACK_PATH}?${encodeSnapshot(snapshot)}`);
};

void run();
