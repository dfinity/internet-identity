import idp_actor, { IDPActor } from "../utils/idp_actor";
import { resetForm } from "../utils/resetForm";
import { setUserId } from "../utils/userId";

function setupRegisterIdentityForm(cb?: (userId: bigint, connection: IDPActor) => void) {
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

    // Send values through actor
    IDPActor.register(registerAlias.value).then(({ connection, userId}) => {
      resetForm(form);
      setUserId(userId);
      idp_actor.connection = connection;
      cb?.(userId, connection)
    }).catch((err) => {
      console.error(err);
      submitButton.removeAttribute("disabled");
    });
    return false;
  };

  form.onsubmit = handleSubmit;
}

export default setupRegisterIdentityForm;
