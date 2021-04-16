import idp_actor from "../utils/idp_actor";
import { resetForm } from "../utils/resetForm";

function setupLookupIdentityForm() {
  const form = document.getElementById("lookupForm") as HTMLFormElement;
  const submitButton = form.querySelector(
    'button[type="submit"]'
  ) as HTMLButtonElement;

  const handleSubmit = (e) => {
    // Enter pending state
    e.preventDefault();
    submitButton.setAttribute("disabled", "true");

    // Read values from inputs
    const lookupUser = form.querySelector("#lookupUser") as HTMLInputElement;

    // Send values through actor
    idp_actor
      .lookup()
      .then((returnValue) => {
        console.info("successfully looked up identity", returnValue);

        // Clean up
        resetForm(form);
      })
      .catch((err) => {
        console.error(err);
        submitButton.removeAttribute("disabled");
      });

    return false;
  };
  form.onsubmit = handleSubmit;
}

export default setupLookupIdentityForm;
