import setupAddIdentityForm from "./components/setupAddIdentityForm";
import setupLookupIdentityForm from "./components/setupLookupIdentityForm";
import setupRegisterIdentityForm from "./components/setupRegisterIdentityForm";

function init() {
  setupRegisterIdentityForm();
  setupAddIdentityForm();
  setupLookupIdentityForm();
}

window.addEventListener("DOMContentLoaded", (event) => {
  init;
});
