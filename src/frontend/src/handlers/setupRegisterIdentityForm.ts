import idp_actor from "../utils/idp_actor";
import { resetForm } from "../utils/resetForm";

function setupRegisterIdentityForm(cb?: () => void) {
  const form = document.getElementById("registerForm") as HTMLFormElement;
  const submitButton = form.querySelector(
    'button[type="submit"]'
  ) as HTMLButtonElement;

  const handleSubmit = (e) => {
    // Enter pending state
    e.preventDefault();
    submitButton.setAttribute("disabled", "true");

    // Read values from inputs
    const registerUser = form.querySelector(
      "#registerUser"
    ) as HTMLInputElement;
    const registerAlias = form.querySelector(
      "#registerAlias"
    ) as HTMLInputElement;

    // Send values through actor
    idp_actor
      .register(BigInt(registerUser.value), registerAlias.value)
      .then((returnValue) => {
        console.info("successfully registered identity", returnValue);

        // Clean up
        resetForm(form);

        // If callback, invoke
        cb?.();
      })
      .catch((err) => {
        console.error(err);
        submitButton.removeAttribute("disabled");
      });

    return false;
  };

  form.onsubmit = handleSubmit;
}

export default setupRegisterIdentityForm;
