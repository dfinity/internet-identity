import { injectCanisterIdAndConfigPlugin } from "@dfinity/internet-identity-vite-plugins";
import { nonNullish } from "@dfinity/utils";
import type { Handle, ServerInit } from "@sveltejs/kit";
import { agentOptions, canisterId, initGlobals } from "$lib/globals";
import featureFlags from "$lib/state/featureFlags";
import { localeStore } from "$lib/stores/locale.store";
import { sessionStore } from "$lib/stores/session.store";
import { authenticationStore } from "$lib/stores/authentication.store";

const transformHtml =
  process.env.NODE_ENV === "development"
    ? (injectCanisterIdAndConfigPlugin({ canisterName: "internet_identity" })
        ?.transformIndexHtml as (html: string) => string)
    : undefined;

export const handle: Handle = async ({ event, resolve }) => {
  const response = await resolve(event);
  if (
    nonNullish(transformHtml) &&
    response.headers.get("Content-Type") === "text/html" &&
    response.ok
  ) {
    const html = await response.text();
    return new Response(transformHtml(html), {
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
