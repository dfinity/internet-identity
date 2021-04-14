import actor from "../utils/actor";

function setupRemoveIdentityForm(props) {
  const form = document.getElementById("removeForm") as HTMLFormElement;

  const handleSubmit = (e) => {
    e.preventDefault();
    const target = e.target as HTMLFormElement;
    const removeUser = target.querySelector("#removeUser") as HTMLInputElement;

    actor.remove(BigInt(removeUser.value));

    return false;
  };

  form.onsubmit = handleSubmit;
}

export default setupRemoveIdentityForm;
