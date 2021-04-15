import setupRegisterIdentityForm from "../handlers/setupRegisterIdentityForm";

export const initNewUser = () => {
  setupRegisterIdentityForm(() => {
    window.location.assign("/manage.html");
  });
};
