import type { APIRoute } from "astro";
import crypto from "crypto";
import {COOKIE_ATTRIBUTES, cookieToSessionStatus, II_SESSION_COOKIE_NAME} from "../../state.ts";

export const GET: APIRoute = () => {
  console.log("... new challenge: challenge requested");
  // challenge is 64 bytes long so that it can act as a public key in DelegationChain
  let challenge = crypto.randomBytes(64).toString("hex");
  let cookie = crypto.randomBytes(4).toString("hex");
  cookieToSessionStatus[cookie] = challenge;

  console.log("... new challenge: challenge:", challenge);
  console.log("... new challenge: ii_session cookie:", cookie);

  let response = new Response(
      JSON.stringify({
        challenge: challenge,
      }),
      {
        status: 200,
      });
  response.headers.set("Set-Cookie", [II_SESSION_COOKIE_NAME + '=' + cookie + '; ' + COOKIE_ATTRIBUTES].toString());
  console.log("... new challenge: set cookies:", response.headers.getSetCookie());
  return response
};
