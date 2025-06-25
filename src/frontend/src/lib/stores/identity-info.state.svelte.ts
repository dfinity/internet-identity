import { get } from "svelte/store";
import {
  DeviceRegistrationInfo,
  DeviceWithUsage,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { authenticatedStore } from "./authentication.store";
import { nonNullish } from "@dfinity/utils";

const fetchIdentityInfo = async () => {
  let authenticated = get(authenticatedStore);

  return await authenticated.actor.get_anchor_info(
    authenticated.identityNumber,
  );
};

class IdentityInfo {
  loaded = $state(false);
  name = $state("");
  devices = $state<DeviceWithUsage[]>([]);
  openIdCredentials = $state<OpenIdCredential[]>([]);
  deviceRegistration = $state<DeviceRegistrationInfo>();

  fetch = async () => {
    try {
      let { name, devices, openid_credentials, device_registration } =
        await fetchIdentityInfo();
      if (nonNullish(name[0])) {
        this.name = name[0];
      }
      if (nonNullish(devices)) {
        this.devices = devices;
      }
      if (nonNullish(openid_credentials[0])) {
        this.openIdCredentials = openid_credentials[0];
      }
      if (nonNullish(device_registration[0])) {
        this.deviceRegistration = device_registration[0];
      }
      this.loaded = true;
    } catch (e) {
      this.reset();
      throw e;
    }
  };

  reset = () => {
    this.name = "";
    this.devices = [];
    this.openIdCredentials = [];
    this.deviceRegistration = undefined;
    this.loaded = false;
  };

  constructor() {}
}

export default new IdentityInfo();
