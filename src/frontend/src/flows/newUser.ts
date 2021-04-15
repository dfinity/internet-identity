import setupRegisterIdentityForm from "../handlers/setupRegisterIdentityForm";

export const initNewUser = () => {
  setupRegisterIdentityForm(() => {
    window.location.assign("/manage.html");
  });

  // TODO - determine whether to pass user ID
  const userInput = document.getElementById("registerUser") as HTMLInputElement;
  const fakeId = Math.random().toString().slice(3, 13);
  userInput.value = fakeId;
};
