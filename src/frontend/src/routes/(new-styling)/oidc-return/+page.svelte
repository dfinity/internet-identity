<script lang="ts">
  import { sessionStore } from "$lib/stores/session.store";
  import { canisterId } from "$lib/globals";
  import {
    authenticateIntermediateWithJWT,
    authenticateWithJWT,
  } from "$lib/utils/authentication";
  import { get } from "svelte/store";
  import { decodeJWT } from "$lib/utils/openID";
  import {
    DelegationChain,
    DelegationIdentity,
    ECDSAKeyIdentity,
  } from "@dfinity/identity";
  import { restoreECDSAIdentity } from "$lib/utils/restoreECDSAIdentity";
  import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
  import { goto } from "$app/navigation";
  import { onMount } from "svelte";

  onMount(async () => {
    // Parse URL hash from OIDC provider redirect
    const url = new URL(window.location.href);
    const hash = new URLSearchParams(url.hash.slice(1));
    const idToken = hash.get("id_token");
    const state = hash.get("state");

    // Restore session data
    const expectedState = sessionStorage.getItem("openid_state");
    const nonce = sessionStorage.getItem("openid_nonce");
    const redirectUri = sessionStorage.getItem("openid_redirect_uri");
    const derivationOrigin = sessionStorage.getItem("openid_derivation_origin");
    const ttlMs = Number(
      sessionStorage.getItem("openid_max_time_to_live") ?? "900000",
    );
    const expiration = new Date(Date.now() + ttlMs);

    try {
      // Validate state
      if (!idToken || state !== expectedState) {
        throw new Error("Invalid state or missing id_token");
      }

      // Validate nonce inside the ID token
      const claims = decodeJWT(idToken);
      if (claims.nonce !== nonce) {
        throw new Error("Invalid nonce");
      }

      // Validate derivation origin
      const requestOrigin = new URL(redirectUri ?? "/").origin;
      const validation = await validateDerivationOrigin({
        requestOrigin,
        derivationOrigin: derivationOrigin ?? undefined,
      });
      if (validation.result === "invalid") {
        throw new Error(validation.message);
      }

      // Generate intermediate key
      const intermediateKey = await ECDSAKeyIdentity.generate();

      // Build identity
      const session = get(sessionStore);
      const intermediateIdentity = await authenticateIntermediateWithJWT({
        canisterId,
        session,
        jwt: idToken,
        intermediateIdentity: intermediateKey,
      });

      // Load app keypair from session storage
      const appKeypairRaw = JSON.parse(
        sessionStorage.getItem("openid_app_keypair") ?? "{}",
      );
      const appIdentity = await restoreECDSAIdentity(appKeypairRaw);
      const appPublicKey = appIdentity.getPublicKey();

      // Step 2: Intermediate → App
      const appDelegationChain = await DelegationChain.create(
        intermediateKey,
        appPublicKey,
        expiration,
        { previous: intermediateIdentity.getDelegation() },
      );

      // Final identity with full chain
      const finalIdentity = DelegationIdentity.fromDelegation(
        intermediateKey,
        appDelegationChain,
      );

      // Redirect to app
      const redirect = new URL(redirectUri ?? "/");
      redirect.hash = `delegation=${encodeURIComponent(
        JSON.stringify(finalIdentity.getDelegation()),
      )}&userKey=${encodeURIComponent(
        Buffer.from(finalIdentity.getPublicKey().toDer()).toString("base64"),
      )}`;

      window.location.href = redirect.toString();
    } catch {
      // TODO: handle correctly
      goto("/authorize");
    }
  });
</script>

<p>Finishing login, please wait…</p>
