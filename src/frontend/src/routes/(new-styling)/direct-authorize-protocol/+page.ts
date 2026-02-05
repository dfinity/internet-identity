import { redirect } from "@sveltejs/kit";
import { canisterConfig } from "$lib/globals";
import { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  const issuer = url.searchParams.get("openid");
  const config = canisterConfig.openid_configs[0]?.find(
    (config) => issuer !== null && config.issuer === issuer,
  );
  if (config === undefined) {
    // If OpenID config can't be found, fallback to ICRC-29 authorization flow
    throw redirect(307, "/authorize");
  }
  return { config };
};
