import { injectCanisterIdAndConfigPlugin } from "@dfinity/internet-identity-vite-plugins";
import type { Handle, ServerInit } from "@sveltejs/kit";
import { localeStore } from "$lib/stores/locale.store";

const transformHtml =
  process.env.NODE_ENV === "development"
    ? (injectCanisterIdAndConfigPlugin({ canisterName: "internet_identity", configCanisterName: "internet_identity_frontend" })
        ?.transformIndexHtml as (html: string) => Promise<string>)
    : undefined;

export const handle: Handle = async ({ event, resolve }) => {
  const response = await resolve(event);
  if (
    transformHtml !== undefined &&
    response.headers.get("Content-Type") === "text/html" &&
    response.ok
  ) {
    const html = await response.text();
    return new Response(await transformHtml(html), {
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
