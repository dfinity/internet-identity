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
    const registerAlias = form.querySelector(
      "#registerAlias"
    ) as HTMLInputElement;

    const alias = registerAlias.value;
    // Send values through actor
    idp_actor
      .register(alias)
      .then((userId) => {
        console.info("successfully registered identity", userId);

        // Store user id and alias in local storage.
        localStorage.setItem("userId", userId.toString());
        localStorage.setItem("alias", alias);

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
