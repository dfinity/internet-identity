import idp_actor from "../utils/idp_actor";
import { resetForm } from "../utils/resetForm";

function setupAddIdentityForm() {
  const form = document.getElementById("addForm") as HTMLFormElement;
  const submitButton = form.querySelector(
    'button[type="submit"]'
  ) as HTMLButtonElement;

  const handleSubmit = (e) => {
    // Enter pending state
    e.preventDefault();
    submitButton.setAttribute("disabled", "true");

    // Read values from inputs
    const addUser = form.querySelector("#addUser") as HTMLInputElement;
    const addAlias = form.querySelector("#addAlias") as HTMLInputElement;

    // Send values through actor
    idp_actor
      .add(BigInt(addUser.value), addAlias.value)
      .then((returnValue) => {
        console.info("successfully added identity", returnValue);

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

export default setupAddIdentityForm;
