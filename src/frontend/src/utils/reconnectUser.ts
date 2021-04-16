import idp_actor from "./idp_actor";
import { redirectBackWithAuthorization } from "./oath";

export const reconnectUser = async (shouldPrompt: boolean) => {
  // Attempt to log user in directly if they have a stored identity
  if (idp_actor.storedIdentity) {
    const existingDelegation = await idp_actor.requestDelegation();
    if (existingDelegation) return redirectBackWithAuthorization();

    const newDelegation = await idp_actor.requestDelegation();
    if (newDelegation) return redirectBackWithAuthorization();
  }

  if (shouldPrompt) {
    //   TODO: Logic for how this works
  }
  return undefined;
};
