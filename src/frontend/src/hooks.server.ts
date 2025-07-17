import { injectCanisterIdAndConfigPlugin } from "@dfinity/internet-identity-vite-plugins";
import { nonNullish } from "@dfinity/utils";
import type { Handle } from "@sveltejs/kit";

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
