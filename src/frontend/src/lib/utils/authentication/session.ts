import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@dfinity/identity";
import { Session } from "$lib/stores/session.store";

export const authenticateWithSession = async ({
  session,
  expiration = 30 * 60 * 1000,
}: {
  session: Pick<Session, "identity">;
  expiration?: number;
}): Promise<DelegationIdentity> => {
  const targetIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const delegation = await DelegationChain.create(
    session.identity,
    targetIdentity.getPublicKey(),
    new Date(Date.now() + expiration),
  );
  return DelegationIdentity.fromDelegation(targetIdentity, delegation);
};
