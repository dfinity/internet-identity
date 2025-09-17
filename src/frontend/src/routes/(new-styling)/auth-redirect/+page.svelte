<script lang="ts">
  import { canisterConfig } from "$lib/globals";
  import { goto } from "$app/navigation";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import type { PageProps } from "./$types";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { sessionStore } from "$lib/stores/session.store";
  import { get } from "svelte/store";
  import { onMount } from "svelte";
  import { ECDSAKeyIdentity } from "@dfinity/identity";

  const { data }: PageProps = $props();
  const { redirectUri, derivationOrigin, appKeypair, provider, maxTimeToLive } =
    data;

  const openIdProviders = canisterConfig.openid_configs?.[0] ?? [];
  let authMethod = openIdProviders.find(
    (p) => p.name.toLowerCase() === provider?.toLowerCase(),
  );

  const startRedirect = async () => {
    if (!authMethod) return;

    const authFlow = new AuthFlow();
    const session = get(sessionStore);
    const { privateKey, publicKey } = (
      session.identity as ECDSAKeyIdentity
    ).getKeyPair();

    const [privJwk, pubJwk] = await Promise.all([
      crypto.subtle.exportKey("jwk", privateKey),
      crypto.subtle.exportKey("jwk", publicKey),
    ]);

    sessionStorage.setItem(
      "openid_ii_keypair",
      JSON.stringify({ privateKey: privJwk, publicKey: pubJwk }),
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

    console.log("📝 Storing session data for OpenID redirect");
    console.log(
      "🔑 II Public Key (DER hex):",
      Buffer.from(session.identity.getPublicKey().toDer()).toString("hex"),
    );
    console.log("🧂 Salt (hex):", Buffer.from(session.salt).toString("hex"));
    console.log("🌀 Nonce:", session.nonce);
    console.log("🔁 Redirect URI:", redirectUri);
    console.log("🌍 Derivation Origin:", derivationOrigin);
    console.log("🔐 App Keypair:", appKeypair);
    console.log("⏳ Max TTL (ms):", maxTimeToLive);

    await authFlow.continueWithOpenIdFullRedirect(authMethod);
  };
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <div class="flex h-dvh w-full flex-col items-center justify-center gap-2">
    {#if authMethod?.name}
      <p class="text-text-primary">Redirecting you to {authMethod.name}</p>
    {/if}
    <ProgressRing class="text-fg-brand-primary" />
    <button
      on:click={startRedirect}
      class="bg-fg-brand-primary hover:bg-fg-brand-primary/80 mt-4 rounded px-6 py-2 text-white transition"
    >
      Continue with {authMethod?.name}
    </button>
  </div>
</div>
