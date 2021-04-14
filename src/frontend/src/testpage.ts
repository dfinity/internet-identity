import setupAddIdentityForm from "./components/setupAddIdentityForm";
import setupLookupIdentityForm from "./components/setupLookupIdentityForm";
import setupRegisterIdentityForm from "./components/setupRegisterIdentityForm";

import "./styles/main.css";

function init() {
  console.log("init");

  setupRegisterIdentityForm();
  setupAddIdentityForm();
  setupLookupIdentityForm();
}

init();
