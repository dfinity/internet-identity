import setupLookupIdentityForm from "./handlers/setupLookupIdentityForm";
import setupRegisterIdentityForm from "./handlers/setupRegisterIdentityForm";

import "./styles/main.css";

function init() {
  console.log("init");

  setupRegisterIdentityForm();
  setupLookupIdentityForm();
}

init();
