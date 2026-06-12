import type { Handle, ServerInit } from "@sveltejs/kit";
import { building } from "$app/environment";
import { localeStore } from "$lib/stores/locale.store";
import { execSync } from "child_process";

/** Origin of the locally deployed frontend canister, via the replica gateway. */
const frontendCanisterOrigin = (): string => {
  const canisterId = execSync(
    "icp canister status internet_identity_frontend --id-only",
  )
    .toString()
    .trim();
  const port = new URL(
    JSON.parse(execSync("icp network status --json").toString()).gateway_url,
  ).port;
  return `http://${canisterId}.localhost:${port}`;
};

export const handle: Handle = async ({ event, resolve }) => {
  // OpenID providers deliver the OAuth response with `response_mode=form_post`:
  // a form POST to `/callback`. In production the frontend canister translates
  // it into a certified HTML page that hands the payload to the frontend. The
  // hot-reload dev server has no canister in front of it, so forward the POST
  // to the deployed canister (always installed when working on OpenID) and
  // return its response — dev exercises the real translator
  // (`src/internet_identity_frontend/src/callback.rs`) rather than a
  // reimplementation. In `NO_HOT_RELOAD` e2e the `replicaForwardPlugin`
  // forwards this POST to the canister before SvelteKit sees it.
  if (
    !building &&
    event.request.method === "POST" &&
    event.url.pathname === "/callback"
  ) {
    const forwarded = await fetch(`${frontendCanisterOrigin()}/callback`, {
      method: "POST",
      headers: {
        "content-type":
          event.request.headers.get("content-type") ??
          "application/x-www-form-urlencoded",
      },
      body: await event.request.text(),
    });
    // `fetch` has already decoded the body, so drop the headers describing the
    // now-absent encoding before handing the response back to the browser.
    const headers = new Headers(forwarded.headers);
    headers.delete("content-encoding");
    headers.delete("content-length");
    return new Response(await forwarded.text(), {
      status: forwarded.status,
      headers,
    });
  }

  const response = await resolve(event);
  if (
    !building &&
    response.headers.get("Content-Type") === "text/html" &&
    response.ok
  ) {
    // Fetch the frontend canister's HTML to recover its <body> tag, which
    // carries the injected canister ID and config.
    const canisterResponse = await fetch(frontendCanisterOrigin());
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
