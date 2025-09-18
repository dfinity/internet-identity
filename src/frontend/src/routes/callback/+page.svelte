<script lang="ts">
  import { sessionStore } from "$lib/stores/session.store";
  import { canisterId } from "$lib/globals";
  import {
    authenticateRedirectCallbackWithJWT,
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
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import { nonNullish } from "@dfinity/utils";
  import AuthWizard from "$lib/components/wizards/auth/AuthWizard.svelte";
  import CreateIdentity from "$lib/components/wizards/auth/views/CreateIdentity.svelte";
  import SolveCaptcha from "$lib/components/wizards/auth/views/SolveCaptcha.svelte";

  const authFlow = new AuthFlow();
  onMount(async () => {
    // Parse URL hash from OIDC provider redirect
    const url = new URL(window.location.href);
    const hash = new URLSearchParams(url.hash.slice(1));
    const idToken = hash.get("id_token");
    const state = hash.get("state");

    // Restore session data
    const iiKeypairRaw = sessionStorage.getItem("openid_ii_keypair");
    console.log("iiKeypairRaw: ", iiKeypairRaw);
    const authMethodRaw = sessionStorage.getItem("openid_auth_method");
    const saltRaw = sessionStorage.getItem("openid_salt");
    const expectedState = sessionStorage.getItem("openid_state");
    const nonce = sessionStorage.getItem("openid_nonce");
    const redirectUri = sessionStorage.getItem("openid_redirect_uri");
    const derivationOrigin = sessionStorage.getItem("openid_derivation_origin");
    const ttlMs = Number(
      sessionStorage.getItem("openid_max_time_to_live") ?? "900000",
    );
    const expiration = new Date(Date.now() + ttlMs);

    if (iiKeypairRaw === null || iiKeypairRaw === "") {
      throw new Error("Missing II keypair in sessionStorage");
    }
    if (saltRaw === null || saltRaw === "") {
      throw new Error("Missing salt in sessionStorage");
    }
    if (authMethodRaw === null || authMethodRaw === "") {
      throw new Error("Missing auth method in sessionStorage");
    }

    const authMethod = JSON.parse(authMethodRaw) as OpenIdConfig;

    const { privateKey: privJwk, publicKey: pubJwk } = JSON.parse(iiKeypairRaw);

    if (!privJwk.kty) throw new Error("Missing `kty` in privateKey JWK");
    if (!pubJwk.kty) throw new Error("Missing `kty` in publicKey JWK");

    const privateKey = await crypto.subtle.importKey(
      "jwk",
      privJwk,
      { name: "ECDSA", namedCurve: "P-256" },
      true,
      ["sign"],
    );

    const publicKey = await crypto.subtle.importKey(
      "jwk",
      {
        crv: pubJwk.crv,
        ext: true,
        key_ops: ["verify"],
        kty: "EC",
        x: pubJwk.x!,
        y: pubJwk.y!,
      },
      { name: "ECDSA", namedCurve: "P-256" },
      true,
      ["verify"],
    );

    console.log(
      "🔑 WebCrypto Public Key (DER hex):",
      Buffer.from(iiKeypairRaw).toString("hex"),
    );

    const identity = await ECDSAKeyIdentity.fromKeyPair({
      privateKey,
      publicKey,
    });
    console.log(identity);

    const salt = Uint8Array.from(Buffer.from(saltRaw, "base64"));

    // try {
    // Validate state
    if (!idToken || state !== expectedState) {
      throw new Error("Invalid state or missing id_token");
    }

    const [headerB64, payloadB64] = idToken.split(".");
    const header = JSON.parse(atob(headerB64));
    const payload = JSON.parse(atob(payloadB64));

    console.log("🔐 JWT Header:", header);
    console.log("📦 JWT Payload:", payload);

    // Validate nonce inside the ID token
    const claims = decodeJWT(idToken);
    if (claims.nonce !== nonce) {
      throw new Error("Invalid nonce");
    }

    console.log("🎯 aud:", claims.aud);
    console.log("🔐 iss:", claims.iss);

    // Validate derivation origin
    const requestOrigin = new URL(redirectUri ?? "/").origin;
    console.log(redirectUri);
    console.log(requestOrigin);
    const validation = await validateDerivationOrigin({
      requestOrigin,
      derivationOrigin: derivationOrigin ?? undefined,
    });
    console.log(validation);
    if (validation.result === "invalid") {
      throw new Error(validation.message);
    }

    // Load app keypair from session storage
    const appKeypairRaw = JSON.parse(
      sessionStorage.getItem("openid_app_keypair") ?? "{}",
    );
    const appIdentity = await restoreECDSAIdentity(appKeypairRaw);
    console.log("AppIdentity: ", appIdentity);

    // Build chain from II delegation
    const result = await authFlow.completeOpenIdFullRedirect(
      authMethod,
      idToken,
      salt,
      identity,
      appIdentity,
    );

    console.log(result);
    console.log(authFlow.view);
    if (result.type === "signIn") {
      const redirect = new URL(redirectUri ?? "/");
      redirect.hash = `identity=${encodeURIComponent(
        JSON.stringify(result.identityNumber.toString()),
      )}&userKey=${encodeURIComponent(
        Buffer.from(identity.getPublicKey().toDer()).toString("base64"),
      )}`;

      window.location.href = redirect.toString();
    } else if (nonNullish(result.name)) {
      console.log(result.name);
      const {
        identity: completeIdentity,
        identityNumber: completeIdentityNumber,
      } = await authFlow.completeRedirectOpenIdRegistration(
        identity,
        result.name,
        salt,
        appIdentity,
      );
      console.log(
        "completeIdentity principal:",
        completeIdentity.getPrincipal().toText(),
      );
      console.log(
        "completeIdentity delegation:",
        JSON.stringify(completeIdentity.getDelegation().toJSON()),
      );

      const completeIdentityJson = JSON.stringify(
        completeIdentity.getDelegation().toJSON(),
      );
      const redirect = new URL(redirectUri ?? "/");
      redirect.hash = `identity=${encodeURIComponent(
        completeIdentityJson,
      )}&userKey=${encodeURIComponent(
        Buffer.from(identity.getPublicKey().toDer()).toString("base64"),
      )}`;

      window.location.href = redirect.toString();
    }
    // Redirect to app

    // } catch {
    //   // TODO: handle correctly
    //   goto("/authorize");
    // }
  });
</script>

{#if nonNullish(authFlow.captcha)}
  <SolveCaptcha {...authFlow.captcha} />
{/if}
<p>Finishing login, please wait…</p>
