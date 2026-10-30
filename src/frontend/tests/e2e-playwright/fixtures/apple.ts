import {
  test as base,
  type BrowserContext,
  type Page,
  type Route,
} from "@playwright/test";

/**
 * The test OpenID provider behind the "Apple" provider: the canister's config
 * for it has Apple's real authorize URL, so the frontend takes its Sign in with
 * Apple paths, and this provider's issuer and keys, so the canister accepts the
 * tokens it issues. See `scripts/dev-e2e-setup`.
 */
export const APPLE_OPENID_PORT = 11110;
export const APPLE_ISSUER = `http://localhost:${APPLE_OPENID_PORT}`;
export const APPLE_PROVIDER_NAME = "Apple";

const APPLE_ORIGIN = "https://appleid.apple.com";
const APPLE_AUTHORIZE_PATH = "/auth/authorize";
/** The one redirect URI the provider on {@link APPLE_OPENID_PORT} accepts. */
const APPLE_REDIRECT_URI = `${APPLE_ORIGIN}/e2e/callback`;

/** Safari on macOS, where Sign in with Apple opens Apple's native sheet. */
export const SAFARI_USER_AGENT =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.5 Safari/605.1.15";

/** Apple's `user`: sent beside the token on a user's first authorization. */
export type AppleUser = {
  name?: { firstName?: string; lastName?: string };
  email?: string;
};

export type FakeAppleConfig = {
  /**
   * Sent beside the token when the user signs in, if the authorization asked
   * for the `name` scope, as Apple does.
   */
  user?: AppleUser;
  /**
   * Take a page's navigation to Apple the way Safari's native sheet does: the
   * page stays where it is, without unloading, until
   * {@link FakeApple.cancelSheet} or {@link FakeApple.completeSheet}.
   */
  sheet?: boolean;
  /** Answer each authorization as if the user canceled Apple's sheet. */
  cancel?: boolean;
  /**
   * Never answer: a canceled sheet can end without any message from Apple,
   * leaving the page that opened the popup only its focus coming back.
   */
  silentCancel?: boolean;
};

export type FakeApple = {
  /** The authorization requests the browser sent to Apple, in order. */
  authorizeRequests: URL[];
  /** Resolves once the sheet took a navigation (see `sheet`). */
  sheetShown: Promise<void>;
  /**
   * The user cancels the sheet: the page loses its focus to the sheet and gets
   * it back, and the navigation never goes on.
   */
  cancelSheet: (page: Page) => Promise<void>;
  /**
   * The user signs in on the sheet: the page loses its focus to the sheet and
   * gets it back, then the navigation goes on, which unloads the page. Apple
   * answers only after `appleResponseDelayMs`.
   */
  completeSheet: (page: Page, appleResponseDelayMs: number) => Promise<void>;
};

const escapeHtml = (value: string): string =>
  value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");

const scriptJson = (value: unknown): string =>
  JSON.stringify(value).replace(/</g, "\\u003c");

const html = (body: string) => ({
  status: 200,
  contentType: "text/html",
  body: `<!doctype html><html><body>${body}</body></html>`,
});

/** Apple's `web_message` reply, posted to the window that opened the popup. */
const webMessagePage = (targetOrigin: string, data: unknown) =>
  html(
    `<script>window.opener.postMessage(${scriptJson(
      JSON.stringify({ method: "oauthDone", data }),
    )}, ${scriptJson(targetOrigin)});</script>`,
  );

/** Apple's `form_post` reply, submitted to the redirect URI. */
const formPostPage = (action: string, fields: Record<string, string>) =>
  html(
    `<form method="post" action="${escapeHtml(action)}">${Object.entries(fields)
      .map(
        ([name, value]) =>
          `<input type="hidden" name="${escapeHtml(name)}" value="${escapeHtml(value)}">`,
      )
      .join("")}</form><script>document.forms[0].submit();</script>`,
  );

/**
 * Stands in for https://appleid.apple.com. An authorization is sent on to the
 * provider on {@link APPLE_OPENID_PORT} as a `form_post` back to Apple's
 * origin, where Apple replies to II the way the authorization asked for:
 * `form_post` to II's `/callback`, or `web_message` to the window that opened
 * the popup, with {@link FakeAppleConfig.user} beside the token.
 */
const installFakeApple = async (
  context: BrowserContext,
  config: FakeAppleConfig,
): Promise<FakeApple> => {
  const authorizeRequests: URL[] = [];
  const replyTo = new Map<
    string,
    { redirectUri: string; responseMode: string; sendsUser: boolean }
  >();
  let heldAuthorization: URL | undefined;
  let released: { href: string; delayMs: number } | undefined;
  let sheetShown: () => void = () => undefined;
  const sheetShownPromise = new Promise<void>((resolve) => {
    sheetShown = resolve;
  });

  const sendToProvider = (route: Route, authorization: URL) => {
    const state = authorization.searchParams.get("state") ?? "";
    const scopes = (authorization.searchParams.get("scope") ?? "").split(" ");
    replyTo.set(state, {
      redirectUri: authorization.searchParams.get("redirect_uri") ?? "",
      responseMode: authorization.searchParams.get("response_mode") ?? "",
      sendsUser: config.user !== undefined && scopes.includes("name"),
    });
    const provider = new URL(`${APPLE_ISSUER}/auth`);
    provider.search = authorization.search;
    provider.searchParams.set("response_mode", "form_post");
    provider.searchParams.set("redirect_uri", APPLE_REDIRECT_URI);
    // `name` is Apple's own scope, which the provider doesn't know. Apple
    // keeps the name out of the token and sends it beside it instead.
    provider.searchParams.set(
      "scope",
      scopes.filter((scope) => scope !== "name").join(" "),
    );
    return route.fulfill({
      status: 302,
      headers: { location: provider.href },
    });
  };

  const onAuthorize = async (route: Route, authorization: URL) => {
    if (authorization.href === released?.href) {
      const { delayMs } = released;
      released = undefined;
      await new Promise((resolve) => setTimeout(resolve, delayMs));
      await sendToProvider(route, authorization);
      return;
    }
    authorizeRequests.push(authorization);
    if (config.sheet === true) {
      heldAuthorization = authorization;
      // A 204 ends the navigation and leaves the page as it is. Playwright
      // waits for a navigation in flight before it reads or runs anything on
      // the page, so the navigation is ended here rather than held, and
      // started again once the sheet completes.
      await route.fulfill({ status: 204 });
      sheetShown();
      return;
    }
    if (config.cancel === true) {
      const redirectUri = authorization.searchParams.get("redirect_uri") ?? "";
      await route.fulfill(
        webMessagePage(new URL(redirectUri).origin, {
          error: "user_cancelled_authorize",
        }),
      );
      return;
    }
    if (config.silentCancel === true) {
      await route.fulfill(html(""));
      return;
    }
    await sendToProvider(route, authorization);
  };

  const onProviderReply = async (route: Route) => {
    const fields = Object.fromEntries(
      new URLSearchParams(route.request().postData() ?? ""),
    );
    const reply = replyTo.get(fields.state ?? "");
    if (reply === undefined) {
      await route.fulfill({ status: 400, body: "Unknown state" });
      return;
    }
    const authorization = {
      code: fields.code ?? "",
      id_token: fields.id_token ?? "",
      state: fields.state ?? "",
    };
    if (reply.responseMode === "web_message") {
      await route.fulfill(
        webMessagePage(new URL(reply.redirectUri).origin, {
          authorization,
          ...(reply.sendsUser ? { user: config.user } : {}),
        }),
      );
      return;
    }
    await route.fulfill(
      formPostPage(reply.redirectUri, {
        ...authorization,
        ...(reply.sendsUser ? { user: JSON.stringify(config.user) } : {}),
      }),
    );
  };

  await context.route(
    (url) => url.origin === APPLE_ORIGIN,
    async (route) => {
      const url = new URL(route.request().url());
      if (url.pathname === APPLE_AUTHORIZE_PATH) {
        await onAuthorize(route, url);
        return;
      }
      if (url.href === APPLE_REDIRECT_URI) {
        await onProviderReply(route);
        return;
      }
      await route.fulfill({ status: 404 });
    },
  );

  if (config.sheet === true) {
    // Safari's native sheet takes the navigation to Apple before the page
    // unloads: no `beforeunload` reaches the page until the user is done with
    // the sheet. Chromium fires it as the navigation starts, so it is held
    // back here until the test lets the navigation go on.
    await context.addInitScript(() => {
      if (window.location.origin !== "https://id.ai") {
        return;
      }
      window.addEventListener("beforeunload", (event) => {
        if (
          (window as { __appleSheetDone?: boolean }).__appleSheetDone !== true
        ) {
          event.stopImmediatePropagation();
        }
      });
    });
  }

  const takeHeld = () => {
    if (heldAuthorization === undefined) {
      throw new Error("No authorization is held by Apple's sheet");
    }
    const held = heldAuthorization;
    heldAuthorization = undefined;
    return held;
  };

  return {
    authorizeRequests,
    sheetShown: sheetShownPromise,
    cancelSheet: async (page) => {
      takeHeld();
      await page.evaluate(() => {
        window.dispatchEvent(new FocusEvent("blur"));
        window.dispatchEvent(new FocusEvent("focus"));
      });
    },
    completeSheet: async (page, appleResponseDelayMs) => {
      const authorization = takeHeld();
      released = { href: authorization.href, delayMs: appleResponseDelayMs };
      // Started from the page, as the page started it: a navigation the
      // browser starts itself would cut the window off from the app that
      // opened it.
      await page.evaluate((href) => {
        window.dispatchEvent(new FocusEvent("blur"));
        window.dispatchEvent(new FocusEvent("focus"));
        (window as { __appleSheetDone?: boolean }).__appleSheetDone = true;
        window.location.assign(href);
      }, authorization.href);
    },
  };
};

export const test = base.extend<{
  fakeAppleConfig: FakeAppleConfig | undefined;
  fakeApple: FakeApple | undefined;
}>({
  fakeAppleConfig: [undefined, { option: true }],
  // Automatic, so the routes are in place before any page reaches Apple and
  // the init script before the II page under test loads.
  fakeApple: [
    async ({ context, fakeAppleConfig }, use) => {
      if (fakeAppleConfig === undefined) {
        await use(undefined);
        return;
      }
      await use(await installFakeApple(context, fakeAppleConfig));
    },
    { auto: true },
  ],
});
