import setupRegisterIdentityForm from "../handlers/setupRegisterIdentityForm";
import oauth from "../utils/oath";

export const initNewUser = () => {
  setupRegisterIdentityForm(() => {
    if (window.location.href.match(/authorize/)) {
      oauth();
    } else {
      window.location.assign("/manage.html");
    }
  });
};
