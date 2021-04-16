import idp_actor from "../utils/idp_actor";
import { resetForm } from "../utils/resetForm";

function setupRemoveIdentityForm(props) {
  const form = document.getElementById("removeForm") as HTMLFormElement;
  const submitButton = form.querySelector(
    'button[type="submit"]'
  ) as HTMLButtonElement;

  const handleSubmit = (e) => {
    // Enter pending state
    e.preventDefault();
    submitButton.setAttribute("disabled", "true");

    const publicKey = Array.from(
      idp_actor.storedIdentity?.getPublicKey()?.toDer() ?? []
    );

    // Send values through actor
    idp_actor
      .remove(publicKey)
      .then((returnValue) => {
        console.info("successfully removed identity", returnValue);

        // Clean up
        resetForm(form);
      })
      .catch((err) => {
        console.error(err);
        submitButton.removeAttribute("disabled");
      });

    // return false;
  };

  form.onsubmit = handleSubmit;
}

export default setupRemoveIdentityForm;
