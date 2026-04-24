import type { Handle, ServerInit } from "@sveltejs/kit";
import { building } from "$app/environment";
import { localeStore } from "$lib/stores/locale.store";
import { execSync } from "child_process";

export const handle: Handle = async ({ event, resolve }) => {
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
