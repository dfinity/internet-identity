import type { APIRoute } from "astro";
import vcUtil, {validateVerifiedAdultPresentation} from "@dfinity/vc-util-web";
import {Principal} from "@dfinity/principal";

import {
  ADULT_VC_ISSUER_ORIGIN,
  cookieToSessionStatus,
  II_SESSION_COOKIE_NAME,
  ROOT_PUBLIC_KEY_RAW
} from "../../../state.ts";


export const POST: APIRoute = async ({ request }) => {
  if (request.headers.get("Content-Type") === "application/json") {
    const body = await request.json();
    // console.log(`--- attribute/verify: submitted verifiable presentation: ${JSON.stringify(body)}`);
    const vpJwt = body["verifiablePresentation"];
    let cookies  = request.headers.get("Cookie");
    console.log("--- attribute/verify: cookies: ", cookies);
    let cookie = getCookie(II_SESSION_COOKIE_NAME, cookies);
    console.log("--- attribute/verify: ii_session cookie: ", cookie);
    if (cookie === undefined) {
      return new Response("missing cookie", { status: 400 })
    }
    let vcSubject = <Principal>cookieToSessionStatus[cookie];
    console.log("--- VC subject:", vcSubject.toText());

    if (vcSubject === undefined) {
      return new Response("II not linked", { status: 400 })
    }
    await verifyAdultVp(vpJwt, vcSubject, ADULT_VC_ISSUER_ORIGIN);
    return new Response(undefined, {
      status: 200,
    });
  }
  return new Response(null, { status: 400 });
};


async function verifyAdultVp(vpJwt : string, vcSubjectPrincipal: Principal, issuerOrigin: string) {
  await vcUtil();
  const iiCanisterId = Principal.fromText("fgte5-ciaaa-aaaad-aaatq-cai");
  const issuerCanisterId = Principal.fromText("v2yvn-myaaa-aaaad-aad4q-cai");

  const currentTimeNs = new Date().getTime().valueOf();
  var encoder = new TextEncoder();
  try {
    validateVerifiedAdultPresentation(encoder.encode(vpJwt),
        vcSubjectPrincipal.toUint8Array(),
        iiCanisterId.toUint8Array(),
        encoder.encode(issuerOrigin),
        issuerCanisterId.toUint8Array(),
        ROOT_PUBLIC_KEY_RAW,
        BigInt(currentTimeNs))
    console.log('--- attribute/verify: Adult VP verified successfully')
  } catch (error) {
    console.error('--- attribute/verify: Adult VP verification failed', error)
  }
}

// Given a cookie `name`, returns the value of the cookie from `cookies` or `null`, if not found.
export function getCookie(name: string, cookies: string | null): string | null {
  if (cookies === null) {
    return null;
  }
  const namePrefixLen = (name.length + 1);
  return cookies
      .split(';')
      .map(c => c.trim())
      .filter(c => {
        return c.substring(0, namePrefixLen) === `${name}=`;
      })
      .map(c => {
        return c.substring(namePrefixLen);
      })[0] || null;
}