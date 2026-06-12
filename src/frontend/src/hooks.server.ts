import type { Handle, ServerInit } from "@sveltejs/kit";
import { building } from "$app/environment";
import { localeStore } from "$lib/stores/locale.store";
import { execSync } from "child_process";

/**
 * Dev-server stand-in for the II frontend canister's POST /callback handler
 * (`src/internet_identity_frontend/src/callback.rs`).
 *
 * OpenID providers deliver the OAuth response with `response_mode=form_post`:
 * a form POST to the `redirect_uri`. In production that URI is served by the
 * frontend canister, which translates the body into an HTML page that hands
 * the payload to the frontend. The hot-reload dev server serves the app
 * directly (no canister in front), so this hook performs the same
 * translation for `http://localhost:5173/callback`.
 *
 * In `NO_HOT_RELOAD` e2e mode this hook never sees the request: the
 * `replicaForwardPlugin` forwards `id.ai` traffic — POSTs included — to the
 * real canister before SvelteKit does, so e2e exercises the production
 * handler.
 */
const formPostCallback = (body: string): Response => {
  const form = new URLSearchParams(body);
  const state = form.get("state");
  const idToken = form.get("id_token");
  const error = form.get("error");
  const errorDescription = form.get("error_description");
  let payload: Record<string, string | null>;
  if (state !== null && idToken !== null) {
    payload = { id_token: idToken, state };
  } else if (state !== null && error !== null) {
    payload = { error, error_description: errorDescription, state };
  } else {
    return new Response(
      "<!doctype html><html><body><p>Sign-in could not be completed. Close this window and try again.</p></body></html>",
      { status: 400, headers: { "Content-Type": "text/html" } },
    );
  }
  // Same payload delivery as the canister page: BroadcastChannel for the
  // popup flow, sessionStorage + authorize-resume navigation for the
  // same-tab flow. JSON goes in a non-executing data block; `<`, `>` and
  // `&` are escaped so it can never contain `</script`.
  const json = JSON.stringify(payload)
    .replaceAll("&", "\\u0026")
    .replaceAll("<", "\\u003c")
    .replaceAll(">", "\\u003e");
  return new Response(
    `<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Internet Identity</title>
</head>
<body>
<script type="application/json" id="cb">${json}</script>
<script>(function () {
  var data = JSON.parse(document.getElementById("cb").textContent);
  if (sessionStorage.getItem("ii-openid-authorize-state") !== null) {
    // 1-click flow: the authorize page navigated this tab to the IdP after
    // stashing its state marker; stash the payload and resume in-app.
    // window.opener can't discriminate the flows here — in the authorize
    // flow this tab is itself a popup opened by the relying party.
    sessionStorage.setItem("ii-openid-callback-data", JSON.stringify(data));
    window.location.replace("/authorize?flow=openid-resume");
  } else {
    // Popup flow: deliver to the tab that opened us and close.
    var channel = new BroadcastChannel("redirect_callback");
    channel.postMessage(data);
    channel.close();
    window.close();
  }
})();</script>
</body>
</html>`,
    {
      status: 200,
      headers: { "Content-Type": "text/html", "Cache-Control": "no-store" },
    },
  );
};

export const handle: Handle = async ({ event, resolve }) => {
  if (
    !building &&
    event.request.method === "POST" &&
    event.url.pathname === "/callback"
  ) {
    return formPostCallback(await event.request.text());
  }
  const response = await resolve(event);
  if (
    !building &&
    response.headers.get("Content-Type") === "text/html" &&
    response.ok
  ) {
    // Get frontend canister id and then fetch it's HTML
    const canisterId = execSync(
      "icp canister status internet_identity_frontend --id-only",
    )
      .toString()
      .trim();
    const port = new URL(
      JSON.parse(execSync("icp network status --json").toString()).gateway_url,
    ).port;
    const canisterResponse = await fetch(
      `http://${canisterId}.localhost:${port}`,
    );
    const canisterHtml = await canisterResponse.text();

    // Replace the body tag in the dev server HTML with the one from the canister HTML
    const bodyTagMatch = canisterHtml.match(/<body [^>]*>/);
    const html = await response.text();
    if (bodyTagMatch == null) {
      throw new Error(
        "Could not find body tag in the frontend canister HTML, cannot inject canister ID and config",
      );
    }
    return new Response(html.replace(/<body [^>]*>/, bodyTagMatch[0]), {
      ...response,
      headers: {
        ...response.headers,
        "Content-Type": "text/html",
      },
    });
  }
  return response;
};

export const init: ServerInit = async () => {
  await localeStore.init();
};
