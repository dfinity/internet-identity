import type { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  return {
    provider: url.searchParams.get("provider"),
    appKeypair: url.searchParams.get("appKeypair"),
    redirectUri: url.searchParams.get("redirectUri"),
    derivationOrigin: url.searchParams.get("derivationOrigin"),
    maxTimeToLive: url.searchParams.get("maxTimeToLive"),
  };
};
