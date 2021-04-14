import actor from "../utils/actor";

function setupAddIdentityForm() {
  const identityForm = document.getElementById("addForm") as HTMLFormElement;

  const handleSubmit = (e) => {
    e.preventDefault();
    const addUser = identityForm.querySelector("#addUser") as HTMLInputElement;
    const addAlias = identityForm.querySelector(
      "#addAlias"
    ) as HTMLInputElement;
    const addPublicKey = identityForm.querySelector(
      "#addPublicKey"
    ) as HTMLInputElement;

    actor.add(
      BigInt(addUser.value),
      addAlias.value,
      addPublicKey.value.split(",").map((num) => Number(num))
    );

    return false;
  };

  identityForm.onsubmit = handleSubmit;
}

export default setupAddIdentityForm;
