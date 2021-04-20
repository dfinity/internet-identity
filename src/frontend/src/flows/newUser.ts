import setupRegisterIdentityForm from "../handlers/setupRegisterIdentityForm";
import oauth from "../utils/oath";
import { navigateTo } from "../utils/router";

export const initNewUser = () => {
  setupRegisterIdentityForm((userId, connection) => {
    if (window.location.href.match(/authorize/)) {
      oauth(userId, connection);
    } else {
      navigateTo("/manage");
    }
  });
};
