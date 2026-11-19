import { get } from "svelte/store";
import { authorizedStore } from "$lib/stores/authorization.store";

/** Finishes Add identity from the header. Once the request was authorized, the sign-in
 *  that answers it goes on only with a new authorization, so the identity added is
 *  authorized with its default account, as Switch identity does. */
export const authorizeAddedIdentity = async (
  authorizeDefault: () => Promise<void>,
): Promise<void> => {
  if (get(authorizedStore) !== undefined) {
    await authorizeDefault();
  }
};
