import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";
import { Session } from "$lib/stores/session.store";

export const authenticateWithSession = async ({
  session,
  expiration = 30 * 60 * 1000,
}: {
  session: Pick<Session, "identity">;
  expiration?: number;
}): Promise<DelegationIdentity> => {
  // Extractable so the auth-handoff utility can export the JWK pair when
  // passing the delegation to a new tab (e.g. "Manage your Internet Identity"
  // popover → cold /manage tab). XSS with code execution can already sign
  // with the identity, so the extractability flag adds no meaningful guard
  // against that threat model.
  const targetIdentity = await ECDSAKeyIdentity.generate({
    extractable: true,
  });
  const delegation = await DelegationChain.create(
    session.identity,
    targetIdentity.getPublicKey(),
    new Date(Date.now() + expiration),
  );
  return DelegationIdentity.fromDelegation(targetIdentity, delegation);
};
