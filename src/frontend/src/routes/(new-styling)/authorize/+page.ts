import { redirect } from "@sveltejs/kit";
import { backendCanisterConfig } from "$lib/globals";
import type { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  const issuer = url.searchParams.get("openid");
  const ssoDomain = url.searchParams.get("sso");

  // `?openid=` and `?sso=` are mutually exclusive — they pick different
  // 1-click entry points and combining them has no well-defined meaning.
  // The layout intercepts this case at `hasError` time and renders the
  // channel-error view, so we just need to bail out of the per-page
  // flow selection here. Returning `flow: "normal"` is harmless — the
  // page never renders when `hasError` is true.
  if (issuer !== null && ssoDomain !== null) {
    return { flow: "normal" as const };
  }

  if (issuer !== null) {
    const config = backendCanisterConfig.openid_configs[0]?.find(
      (config) => config.issuer === issuer,
    );
    if (config === undefined) {
      // If OpenID config can't be found, fallback to ICRC-29 authorization flow
      throw redirect(307, "/authorize");
    }
    return { flow: "openid-init" as const, config };
  }

  if (ssoDomain !== null) {
    const trimmed = ssoDomain.trim().toLowerCase();
    if (trimmed.length === 0) {
      throw redirect(307, "/authorize");
    }
    // No client-side allowlist check — the canister's
    // `add_discoverable_oidc_config` is the trust boundary and traps on
    // anything not on `sso_discoverable_domains`. A non-allowlisted
    // `?sso=` URL surfaces as the error page rather than silently
    // falling back, which is the right signal: the dapp built the URL
    // pointing at a domain II hasn't blessed.
    return { flow: "sso-init" as const, domain: trimmed };
  }

  const flow = url.searchParams.get("flow");
  if (flow === "openid-resume") {
    return { flow: "openid-resume" as const };
  }

  return { flow: "normal" as const };
};
