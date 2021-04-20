import setupRegisterIdentityForm from "../handlers/setupRegisterIdentityForm";
import oauth from "../utils/oath";
import { navigateTo } from "../utils/router";

export const initNewUser = () => {
  setupRegisterIdentityForm(() => {
    if (window.location.href.match(/authorize/)) {
      oauth();
    } else {
      navigateTo("/manage");
    }
  });
};
