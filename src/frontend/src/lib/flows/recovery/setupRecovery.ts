import type { DeviceData } from "$lib/generated/internet_identity_types";
import { withLoader } from "$lib/templates/loader";
import { fromMnemonicWithoutValidation } from "$lib/legacy/crypto/ed25519";
import { generate } from "$lib/legacy/crypto/mnemonic";
import { DOMAIN_COMPATIBILITY } from "$lib/state/featureFlags";
import { get } from "svelte/store";
import { getCredentialsOrigin } from "$lib/utils/credential-devices";
import {
  AuthenticatedConnection,
  creationOptions,
  IC_DERIVATION_PATH,
} from "$lib/utils/iiConnection";
import { userSupportsWebauthRoR } from "$lib/utils/rorSupport";
import { unreachable, unreachableLax } from "$lib/utils/utils";
import { WebAuthnIdentity } from "$lib/utils/webAuthnIdentity";
import { DerEncodedPublicKey, SignIdentity } from "@dfinity/agent";
import { nonNullish } from "@dfinity/utils";
import { confirmSeedPhrase } from "./confirmSeedPhrase";
import { displaySeedPhrase } from "./displaySeedPhrase";

// Set up a recovery device
export const setupKey = async ({
  devices: devices_,
  connection,
}: {
  // When provided, use these devices for exclusion (webauthn) instead of looking up devices
  // (avoids a request saves a couple seconds when used)
  devices?: Omit<DeviceData, "alias">[];
  connection: AuthenticatedConnection;
}): Promise<"ok" | { error: unknown }> => {
  const name = "Recovery key";
  try {
    // Create the WebAuthn credentials and upload them to the canister
    await withLoader(async () => {
      const devices =
        devices_ ?? (await connection.lookupAll(connection.userNumber));
      const newDeviceOrigin =
        userSupportsWebauthRoR() && get(DOMAIN_COMPATIBILITY)
          ? getCredentialsOrigin({
              credentials: devices,
            })
          : undefined;
      const rpId = nonNullish(newDeviceOrigin)
        ? new URL(newDeviceOrigin).host
        : undefined;
      const recoverIdentity = await WebAuthnIdentity.create({
        publicKey: creationOptions(devices, "cross-platform", rpId),
      });

      await connection.add(
        name,
        { cross_platform: null },
        { recovery: null },
        recoverIdentity.getPublicKey().toDer(),
        { unprotected: null },
        newDeviceOrigin ?? window.location.origin,
        recoverIdentity.rawId,
      );
    });
  } catch (error: unknown) {
    return { error };
  }

  return "ok";
};

// Set up a recovery phrase
export const setupPhrase = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  origin: string,
): Promise<"ok" | "error" | "canceled"> => {
  const res = await phraseWizard({
    userNumber,
    operation: "create",
    uploadPhrase: (pubkey) =>
      withLoader(() =>
        connection.add(
          "Recovery phrase",
          { seed_phrase: null },
          { recovery: null },
          pubkey,
          { unprotected: null },
          origin,
        ),
      ),
  });

  if ("ok" in res) {
    return "ok";
  } else if ("error" in res) {
    return "error";
  } else {
    res satisfies { canceled: void };
    return "canceled";
  }
};

// Set up a recovery phrase
export const phraseWizard = async ({
  userNumber,
  operation,
  uploadPhrase,
}: {
  userNumber: bigint;
  operation: "create" | "reset";
  uploadPhrase: (pubkey: DerEncodedPublicKey) => Promise<void>;
}): Promise<{ ok: SignIdentity } | { error: unknown } | { canceled: void }> => {
  const seedPhrase = generate().trim();
  const recoverIdentity = await fromMnemonicWithoutValidation(
    seedPhrase,
    IC_DERIVATION_PATH,
  );

  const phrase = userNumber.toString(10) + " " + seedPhrase;
  const res = await displayAndConfirmPhrase({ phrase, operation });

  if (res === "canceled") {
    return { canceled: undefined };
  }

  res satisfies "confirmed";

  try {
    const pubkey = recoverIdentity.getPublicKey().toDer();
    await withLoader(() => uploadPhrase(pubkey));
    return { ok: recoverIdentity };
  } catch (error: unknown) {
    return { error };
  }
};

// Show the new recovery phrase and ask for confirmation
export const displayAndConfirmPhrase = async ({
  operation,
  phrase,
}: {
  operation: "create" | "reset";
  phrase: string;
}): Promise<"confirmed" | "canceled"> => {
  // Loop until the user has confirmed the phrase
  for (;;) {
    const displayResult = await displaySeedPhrase({
      seedPhrase: phrase,
      operation,
    });
    // User has canceled, so we return
    if (displayResult === "canceled") {
      return "canceled";
    }

    if (displayResult !== "ok") {
      // According to typescript, should never happen
      return unreachable(displayResult, "unexpected return value");
    }

    const result = await confirmSeedPhrase({ phrase });
    // User has confirmed, so break out of the loop
    if (result === "confirmed") {
      return "confirmed";
    }

    // User has clicked the back button, so we retry
    if (result === "back") {
      continue;
    }

    unreachableLax(result);
  }
};
