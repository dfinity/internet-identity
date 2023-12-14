import type { APIRoute } from "astro";
import crypto from "crypto";

export const GET: APIRoute = () => {
  console.log("challenge requested");
  return new Response(
    JSON.stringify({
      challenge: crypto.randomBytes(20).toString("base64"),
    }),
    {
      status: 200,
    },
  );
};
