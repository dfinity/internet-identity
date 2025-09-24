import type { PageLoad } from "./$types";
import { redirect } from "@sveltejs/kit";
import { nonNullish } from "@dfinity/utils";
import { canisterConfig } from "$lib/globals";
import { createRedirectURL } from "$lib/utils/openID";
import { get } from "svelte/store";
import { persistentSessionStore } from "$lib/stores/persistent-session.store";

export const load: PageLoad = async ({ url }) => {
  // Skip interface when OpenID provider is provided, directly redirect first
  const openIdProvider = url.searchParams.get("openid");
  const openIdConfig = nonNullish(openIdProvider)
    ? canisterConfig.openid_configs?.[0]?.find(
        (provider) =>
          provider.name.toLowerCase() === openIdProvider.toLowerCase(),
      )
    : undefined;
  if (nonNullish(openIdConfig)) {
    await persistentSessionStore.reset(); // Always reset before first use
    const redirectURL = createRedirectURL(
      {
        clientId: openIdConfig.client_id,
        authURL: openIdConfig.auth_uri,
        authScope: openIdConfig.auth_scope.join(" "),
      },
      {
        nonce: get(persistentSessionStore).nonce,
        mediation: "required",
      },
    );
    const redirectState = redirectURL.searchParams.get("state")!;
    sessionStorage.setItem("ii-oauth-redirect-state", redirectState);
    throw redirect(307, redirectURL);
  }

  throw redirect(307, "/authorize?rpc");
};
