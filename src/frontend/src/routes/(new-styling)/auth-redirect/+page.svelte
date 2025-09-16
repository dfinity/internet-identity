<script lang="ts">
  import { canisterConfig } from "$lib/globals";
  import { goto } from "$app/navigation";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import type { PageProps } from "./$types";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import { handleError } from "$lib/components/utils/error";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { sessionStore } from "$lib/stores/session.store";
  import { get } from "svelte/store";

  const { data }: PageProps = $props();
  const { redirectUri, derivationOrigin, appKeypair, provider, maxTimeToLive } =
    data;

  const openIdProviders = canisterConfig.openid_configs?.[0] ?? [];

  const authMethod = openIdProviders.find(
    (p) => p.name.toLowerCase() === provider?.toLowerCase(),
  );

  const authFlow = new AuthFlow();

  const handleContinueWithOpenId = async (
    config: OpenIdConfig,
  ): Promise<void | "cancelled"> => {
    try {
      await authFlow.continueWithOpenIdFullRedirect(config);
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return "cancelled";
      }
      handleError(error);
    }
  };

  if (authMethod) {
    const session = get(sessionStore);

    sessionStorage.setItem(
      "openid_ii_keypair",
      JSON.stringify({
        publicKey: Buffer.from(
          session.identity.getPublicKey().toDer(),
        ).toString("base64"),
        principal: session.identity.getPrincipal().toText(),
      }),
    );
    sessionStorage.setItem(
      "openid_salt",
      Buffer.from(session.salt).toString("base64"),
    );
    sessionStorage.setItem("openid_nonce", session.nonce);

    sessionStorage.setItem("openid_redirect_uri", redirectUri ?? "");
    sessionStorage.setItem("openid_derivation_origin", derivationOrigin ?? "");
    sessionStorage.setItem(
      "openid_app_keypair",
      JSON.stringify(appKeypair ?? {}),
    );
    sessionStorage.setItem(
      "openid_max_time_to_live",
      maxTimeToLive?.toString() ?? "",
    );

    handleContinueWithOpenId(authMethod);
  } else {
    // If an incorrect provider is provided, redirect the user to the default authorize flow page.
    // TODO: handle correctly
    goto("/authorize");
  }
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <div class="flex h-dvh w-full flex-col items-center justify-center gap-2">
    {#if authMethod?.name}
      <p class="text-text-primary">Redirecting you to {authMethod.name}</p>
    {/if}
    <ProgressRing class="text-fg-brand-primary" />
  </div>
</div>
