import type { APIRoute } from "astro";

export const POST: APIRoute = async ({ request }) => {
  if (request.headers.get("Content-Type") === "application/json") {
    const body = await request.json();
    console.log(`submitted verifiable presentation: ${JSON.stringify(body)}`);
    return new Response(undefined, {
      status: 200,
    });
  }
  return new Response(null, { status: 400 });
};
