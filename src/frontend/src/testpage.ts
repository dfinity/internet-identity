import setupAddIdentityForm from "./handlers/setupAddIdentityForm";
import setupLookupIdentityForm from "./handlers/setupLookupIdentityForm";
import setupRegisterIdentityForm from "./handlers/setupRegisterIdentityForm";

import "./styles/main.css";

function init() {
  console.log("init");

  setupRegisterIdentityForm();
  setupAddIdentityForm();
  setupLookupIdentityForm();
}

init();
