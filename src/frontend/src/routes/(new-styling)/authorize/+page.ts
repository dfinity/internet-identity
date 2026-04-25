import { redirect } from "@sveltejs/kit";
import { backendCanisterConfig } from "$lib/globals";
import type { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  const issuer = url.searchParams.get("openid");
  const ssoDomain = url.searchParams.get("sso");

  // `?openid=` and `?sso=` are mutually exclusive — they pick different
  // 1-click entry points and combining them has no well-defined meaning.
  // Surface this on the page rather than picking one silently so the
  // dapp gets a clear signal to fix its sign-in URL.
  if (issuer !== null && ssoDomain !== null) {
    return {
      flow: "error" as const,
      reason: "conflicting-params" as const,
    };
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
    const allowlist = backendCanisterConfig.sso_discoverable_domains[0] ?? [];
    if (
      !allowlist.some((entry) => entry.toLowerCase() === trimmed) ||
      trimmed.length === 0
    ) {
      // Mirror the OpenID fallback: an unallowlisted SSO domain should
      // not silently kick off discovery. Drop back to the regular auth
      // flow so the dapp surfaces its own retry path rather than us
      // showing an error page on what's effectively a misconfigured URL.
      throw redirect(307, "/authorize");
    }
    return { flow: "sso-init" as const, domain: trimmed };
  }

  const flow = url.searchParams.get("flow");
  if (flow === "openid-resume") {
    return { flow: "openid-resume" as const };
  }

  return { flow: "normal" as const };
};
