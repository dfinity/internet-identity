import { redirect } from "@sveltejs/kit";
import { backendCanisterConfig } from "$lib/globals";
import type { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  const issuer = url.searchParams.get("openid");
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

  const flow = url.searchParams.get("flow");
  if (flow === "openid-resume") {
    return { flow: "openid-resume" as const };
  }

  return { flow: "normal" as const };
};
