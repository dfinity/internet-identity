import {
  DeviceKey,
  IdentityInfo,
} from "$lib/generated/internet_identity_types";
import { agentOptions, anonymousActor } from "$lib/globals";
import {
  fromMnemonicWithoutValidation,
  IC_DERIVATION_PATH,
  isValidMnemonic,
} from "$lib/utils/recoveryPhrase";
import { throwCanisterError } from "$lib/utils/utils";
import { DerEncodedPublicKey, HttpAgent, SignIdentity } from "@dfinity/agent";
import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";

const authenticateWithRecoveryPhrase = async (
  identity: SignIdentity,
): Promise<DelegationIdentity> => {
  const sessionKey = await ECDSAKeyIdentity.generate({ extractable: false });
  const tenMinutesInMsec = 10 * 1000 * 60;
  const chain = await DelegationChain.create(
    identity,
    sessionKey.getPublicKey(),
    new Date(Date.now() + tenMinutesInMsec),
  );
  return DelegationIdentity.fromDelegation(sessionKey, chain);
};

const bufferEqual = (buf1: ArrayBuffer, buf2: ArrayBuffer): boolean => {
  if (buf1.byteLength !== buf2.byteLength) return false;
  const dv1 = new Int8Array(buf1);
  const dv2 = new Int8Array(buf2);
  for (let i = 0; i !== buf1.byteLength; i++) {
    if (dv1[i] !== dv2[i]) return false;
  }
  return true;
};

const derFromPubkey = (pubkey: DeviceKey): DerEncodedPublicKey =>
  new Uint8Array(pubkey).buffer as DerEncodedPublicKey;

export class InvalidMnemonicError extends Error {
  constructor() {
    super("Invalid mnemonic");
  }
}

export const recoverWithPhrase = async (
  words: string[],
): Promise<{
  info: IdentityInfo;
  identity: DelegationIdentity;
  identityNumber: bigint;
}> => {
  const recoveryPhrase = words.join(" ");
  if (!isValidMnemonic(recoveryPhrase)) {
    throw new InvalidMnemonicError();
  }
  const identity = await fromMnemonicWithoutValidation(
    recoveryPhrase,
    IC_DERIVATION_PATH,
  );
  // TODO: Use lookup endpoint of recovery phrase to user number.
  const userNumber = BigInt(window.prompt("Identity number")!);
  const devices = await anonymousActor.get_anchor_credentials(userNumber);
  const isCorrectPhrase = devices.recovery_phrases.some((pubkey) =>
    bufferEqual(identity.getPublicKey().toDer(), derFromPubkey(pubkey)),
  );
  if (!isCorrectPhrase) {
    throw new Error("Invalid phrase");
  }
  const delegationIdentity = await authenticateWithRecoveryPhrase(identity);
  const agent = HttpAgent.createSync({
    ...agentOptions,
    identity: delegationIdentity,
  });
  // Make call to lookup endpoint
  const identityInfo = await anonymousActor.identity_info
    .withOptions({ agent })(userNumber)
    .then(throwCanisterError);
  return {
    info: identityInfo,
    identity: delegationIdentity,
    identityNumber: userNumber,
  };
};
