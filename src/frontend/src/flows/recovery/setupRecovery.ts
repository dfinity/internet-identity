import { WebAuthnIdentity } from "@dfinity/identity";
import { withLoader } from "../../components/loader";
import { fromMnemonic } from "../../crypto/ed25519";
import { generateMnemonic } from "../../crypto/mnemonic";
import { IC_DERIVATION_PATH, IIConnection } from "../../utils/iiConnection";
import { chooseRecoveryMechanism } from "./chooseRecoveryMechanism";
import { displaySeedPhrase } from "./displaySeedPhrase";
import * as tweetnacl from "tweetnacl";

export const setupRecovery = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  const devices = await IIConnection.lookupAll(userNumber);
  const recoveryMechanism = await chooseRecoveryMechanism(devices);
  if (recoveryMechanism === null) {
    return;
  }

  switch (recoveryMechanism) {
    case "securityKey": {
      const name = "Recovery key";
      const recoverIdentity = await WebAuthnIdentity.create({
        publicKey: {
          authenticatorSelection: {
            userVerification: "preferred",
            authenticatorAttachment: "cross-platform",
          },
          excludeCredentials: devices.flatMap((device) =>
            device.credential_id.length === 0
              ? []
              : {
                  id: new Uint8Array(device.credential_id[0]),
                  type: "public-key",
                }
          ),
          attestation: "direct",
          challenge: Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)),
          pubKeyCredParams: [
            {
              type: "public-key",
              // alg: PubKeyCoseAlgo.ECDSA_WITH_SHA256
              alg: -7,
            },
          ],
          rp: {
            name: "Internet Identity Service",
          },
          user: {
            id: tweetnacl.randomBytes(16),
            name: "Internet Identity",
            displayName: "Internet Identity",
          },
        },
      });
      return await withLoader(() =>
        connection.add(
          userNumber,
          name,
          { cross_platform: null },
          { recovery: null },
          recoverIdentity.getPublicKey().toDer(),
          recoverIdentity.rawId
        )
      );
    }
    case "seedPhrase": {
      const name = "Recovery phrase";
      const seedPhrase = generateMnemonic().trim();
      await displaySeedPhrase(seedPhrase);
      const recoverIdentity = await fromMnemonic(
        seedPhrase,
        IC_DERIVATION_PATH
      );
      await withLoader(() =>
        connection.add(
          userNumber,
          name,
          { seed_phrase: null },
          { recovery: null },
          recoverIdentity.getPublicKey().toDer()
        )
      );
    }
  }
};
