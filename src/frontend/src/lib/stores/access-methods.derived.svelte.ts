import { getLastUsedAccessMethod } from "$lib/utils/accessMethods";
import type {
  AuthnMethodData,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { ENABLE_GENERIC_OPEN_ID } from "$lib/state/featureFlags";
import { canisterConfig } from "$lib/globals";
import identityInfo from "./identity-info.state.svelte";

const MAX_PASSKEYS = 8;

export class AccessMethodsDerived {
  constructor(
    private readonly identityInfo: {
      authnMethods: AuthnMethodData[];
      openIdCredentials: OpenIdCredential[];
    },
  ) {}

  lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(
      this.identityInfo.authnMethods,
      this.identityInfo.openIdCredentials,
    ),
  );

  isMaxOpenIdCredentialsReached = $derived(
    ENABLE_GENERIC_OPEN_ID
      ? this.identityInfo.openIdCredentials.length >=
          (canisterConfig.openid_configs[0]?.length ?? 0)
      : this.identityInfo.openIdCredentials.length >= 1,
  );

  isMaxPasskeysReached = $derived(
    this.identityInfo.authnMethods.length >= MAX_PASSKEYS,
  );

  accessMethodsMaxReached = $derived(
    this.isMaxOpenIdCredentialsReached && this.isMaxPasskeysReached,
  );
}

export const accessMethods = new AccessMethodsDerived(identityInfo);
