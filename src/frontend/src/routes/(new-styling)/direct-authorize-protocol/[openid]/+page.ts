import { redirect } from "@sveltejs/kit";
import { canisterConfig } from "$lib/globals";
import { isNullish } from "@dfinity/utils";

export const load = ({ params }) => {
  const config = canisterConfig.openid_configs[0]?.find(
    (config) => config.name.toLowerCase() === params.openid.toLowerCase(),
  );
  if (isNullish(config)) {
    // If OpenID config can't be found, fallback to original authorization flow
    throw redirect(307, "/authorize");
  }
  return { config };
};
