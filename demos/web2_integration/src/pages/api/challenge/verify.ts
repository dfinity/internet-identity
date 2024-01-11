import type { APIRoute } from "astro";
import {Principal} from "@dfinity/principal";
import {COOKIE_ATTRIBUTES, cookieToSessionStatus, II_SESSION_COOKIE_NAME} from "../../../state.ts";
import crypto from "crypto";
import {DelegationChain, isDelegationValid} from "@dfinity/identity";
import {getCookie} from "../attribute/verify.ts";

export const POST: APIRoute = async ({ request }) => {
  if (request.headers.get("Content-Type") === "application/json") {
    const body = await request.json();
    // console.log(`+++ challenge/verify: submitted delegation chain: ${JSON.stringify(body)}`);
    let cookies  = request.headers.get("Cookie");
    console.log("+++ challenge/verify: cookies: ", cookies);
    let cookie = getCookie(II_SESSION_COOKIE_NAME, cookies);
    if (cookie === undefined) {
      return new Response("missing ii_session cookie", { status: 400 })
    }
    let delegation = DelegationChain.fromJSON(body);
    let challenge = cookieToSessionStatus[cookie];
    if (typeof challenge != "string") {
      return new Response("missing challenge", { status: 400 })
    }
    console.log('+++ challenge/verify: ii_session cookie: ', cookie);
    console.log('+++ challenge/verify: challenge: ', challenge);
    let principal = await verifyDelegation(delegation, challenge);
    // Refresh cookie
    let new_cookie : string = crypto.randomBytes(4).toString("hex");
    cookieToSessionStatus[new_cookie] = principal;
    console.log('+++ challenge/verify: new cookie: ', new_cookie);
    let response = new Response("challenge-verification", {
      status: 200,
    });
    response.headers.set("Set-Cookie", [II_SESSION_COOKIE_NAME + '=' + new_cookie + '; ' + COOKIE_ATTRIBUTES ].toString());
    console.log("+++ challenge/verify: re-set cookies:", response.headers.getSetCookie());
    return response
  }
  return new Response(null, { status: 400 });
};

// @ts-ignore
async function verifyDelegation(delegation: DelegationChain, challenge: string) : Promise<Principal> {
  // load wasm module
  // await initSigVerifier();
  try {
    // NOTE: isDelegationValid does not verify the signatures yet, just the expiration
    // TODO: extend isDelegationValid to do full validation
    isDelegationValid(delegation);
    console.log('+++ challenge/verify: delegation verified successfully');
    let principal = Principal.selfAuthenticating(<Uint8Array>delegation.publicKey.slice(0));
    console.log('+++ challenge/verify: principal: ', principal.toText());
    return principal;
  } catch (error) {
    // the library throws an error if the signature is invalid
    console.error('+++ challenge/verify: delegation verification failed', error)
  }
}
