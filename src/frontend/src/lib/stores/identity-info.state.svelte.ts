import { get } from "svelte/store";
import {
  AuthnMethodData,
  AuthnMethodRegistrationInfo,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { authenticatedStore } from "./authentication.store";
import { nonNullish } from "@dfinity/utils";

const fetchIdentityInfo = async () => {
  let authenticated = get(authenticatedStore);

  let identityInfoResponse = await authenticated.actor.identity_info(
    authenticated.identityNumber,
  );

  console.log(identityInfoResponse);

  if ("Err" in identityInfoResponse)
    throw Error("Failed to fetch identity info");

  return identityInfoResponse.Ok;
};

class IdentityInfo {
  loaded = $state(false);
  name = $state("");
  authnMethods = $state<AuthnMethodData[]>([]);
  openIdCredentials = $state<OpenIdCredential[]>([]);
  authnMethodRegistration = $state<AuthnMethodRegistrationInfo>();

  fetch = async () => {
    try {
      let {
        name,
        openid_credentials,
        authn_methods,
        authn_method_registration,
      } = await fetchIdentityInfo();
      if (nonNullish(name[0])) {
        this.name = name[0];
      }
      if (nonNullish(authn_methods)) {
        this.authnMethods = authn_methods;
      }
      if (nonNullish(openid_credentials[0])) {
        this.openIdCredentials = openid_credentials[0];
      }
      if (nonNullish(authn_method_registration[0])) {
        this.authnMethodRegistration = authn_method_registration[0];
      }
      this.loaded = true;
    } catch (e) {
      this.reset();
      throw e;
    }
  };

  reset = () => {
    this.name = "";
    this.authnMethods = [];
    this.openIdCredentials = [];
    this.authnMethodRegistration = undefined;
    this.loaded = false;
  };

  constructor() {}
}

export default new IdentityInfo();
