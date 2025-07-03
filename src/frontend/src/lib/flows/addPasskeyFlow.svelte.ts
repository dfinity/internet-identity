import {
  DeviceData,
  MetadataMapV2,
  UserNumber,
} from "$lib/generated/internet_identity_types";
import { canisterConfig } from "$lib/globals";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import featureFlags from "$lib/state/featureFlags";
import {
  authenticatedStore,
  authenticationStore,
} from "$lib/stores/authentication.store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { sessionStore } from "$lib/stores/session.store";
import {
  authenticateWithPasskey,
  authenticateWithSession,
} from "$lib/utils/authentication";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { throwCanisterError } from "$lib/utils/utils";
import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import { nonNullish } from "@dfinity/utils";
import { get } from "svelte/store";

export class AddPasskeyFlow {
  view = $state<"loading" | "show-code" | "add-device" | "success">("loading");
  verificationCode: string | undefined;
  #identityNumber: UserNumber;
  #tentativeDevice: DeviceData;
  #pollForVerified;

  constructor(identityNumber: UserNumber) {
    this.#identityNumber = identityNumber;
    const identity = get(sessionStore).identity;
    this.#tentativeDevice = {
      alias: "temporary-key",
      metadata: [],
      origin: [window.location.origin],
      protection: { unprotected: null },
      // TODO: Confirm it works when removing it
      pubkey: Array.from(new Uint8Array(identity.getPublicKey().toDer())),
      purpose: { authentication: null },
      key_type: { unknown: null },
      // TODO: Do we need to set it?
      credential_id: [],
    };
    this.verificationCode = undefined;
    this.#pollForVerified = setInterval(async () => {
      const verifiedResponse =
        await get(sessionStore).actor.authn_method_poll_for_verified(
          identityNumber,
        );
      if ("Ok" in verifiedResponse && verifiedResponse.Ok === true) {
        this.view = "add-device";
        clearInterval(this.#pollForVerified);
      }
      if ("Err" in verifiedResponse) {
        console.log(verifiedResponse.Err);
      }
    }, 2000);
  }

  addTemporaryKey = async () => {
    console.log("adding temporary key");
    const response = await get(sessionStore).actor.add_tentative_device(
      this.#identityNumber,
      this.#tentativeDevice,
    );
    console.log(response);
    if ("added_tentatively" in response) {
      this.verificationCode = response.added_tentatively.verification_code;
      this.view = "show-code";
    }
  };

  addPasskey = async () => {
    const tempPubKey = new Uint8Array(
      get(sessionStore).identity.getPublicKey().toDer(),
    );

    const identityNumber = this.#identityNumber;

    const identityInfoResponse =
      await get(sessionStore).actor.identity_info(identityNumber);

    const { name } = await throwCanisterError(identityInfoResponse);

    const passkeyIdentity =
      featureFlags.DUMMY_AUTH || nonNullish(canisterConfig.dummy_auth[0]?.[0])
        ? await DiscoverableDummyIdentity.createNew(name[0] ?? "")
        : await DiscoverablePasskeyIdentity.createNew(name[0] ?? "");

    const uaParser = loadUAParser();
    const authenticatorAttachment =
      passkeyIdentity.getAuthenticatorAttachment();

    const deviceName = await inferPasskeyAlias({
      authenticatorType: authenticatorAttachment,
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: passkeyIdentity.getAaguid(),
    });

    const origin = window.location.origin;
    // The canister only allow for 50 characters, so for long domains we don't attach an origin
    // (those long domains are most likely a testnet with URL like <canister id>.large03.testnet.dfinity.network, and we basically only care about identity.ic0.app & identity.internetcomputer.org).
    const sanitizedOrigin =
      nonNullish(origin) && origin.length <= 50 ? origin : undefined;

    const metadata: MetadataMapV2 = [
      [
        "alias",
        {
          String: deviceName,
        },
      ],
    ];
    if (nonNullish(authenticatorAttachment)) {
      metadata.push([
        "authenticator_attachment",
        {
          String: authenticatorAttachment,
        },
      ]);
    }
    if (nonNullish(sanitizedOrigin)) {
      metadata.push([
        "origin",
        {
          String: sanitizedOrigin,
        },
      ]);
    }

    let replaceResult = await get(sessionStore).actor.authn_method_replace(
      identityNumber,
      tempPubKey,
      {
        security_settings: {
          protection: { Unprotected: null },
          purpose: { Authentication: null },
        },
        metadata,
        last_authentication: [],
        authn_method: {
          WebAuthn: {
            pubkey: new Uint8Array(passkeyIdentity.getPublicKey().toDer()),
            credential_id: new Uint8Array(passkeyIdentity.getCredentialId()!),
          },
        },
      },
    );

    throwCanisterError(replaceResult);

    const credentialId = new Uint8Array(passkeyIdentity.getCredentialId()!);
    const identity = DelegationIdentity.fromDelegation(passkeyIdentity);

    authenticationStore.set({ identity, identityNumber });
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber,
      name: passkeyIdentity.getName(),
      authMethod: { passkey: { credentialId } },
    });

    console.log(await get(authenticatedStore));
    return replaceResult;
  };
}
