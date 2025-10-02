import { redirect } from "@sveltejs/kit";
import { canisterConfig } from "$lib/globals";
import { isNullish, nonNullish } from "@dfinity/utils";
import { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  const openid = url.searchParams.get("openid");
  const config = canisterConfig.openid_configs[0]?.find(
    (config) =>
      nonNullish(openid) && config.name.toLowerCase() === openid.toLowerCase(),
  );
  if (isNullish(config)) {
    // If OpenID config can't be found, fallback to ICRC-29 authorization flow
    throw redirect(307, "/authorize");
  }
  return { config };
};
