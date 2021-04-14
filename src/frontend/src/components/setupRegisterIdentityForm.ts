import actor from "../utils/actor";

function setupRegisterIdentityForm() {
  const form = document.getElementById("registerForm") as HTMLFormElement;

  const handleSubmit = (e) => {
    e.preventDefault();
    const target = e.target as HTMLFormElement;
    const registerUser = target.querySelector(
      "#registerUser"
    ) as HTMLInputElement;
    const registerAlias = target.querySelector(
      "#registerAlias"
    ) as HTMLInputElement;
    const registerPublicKey = target.querySelector(
      "#registerPublicKey"
    ) as HTMLInputElement;

    actor.register(
      BigInt(registerUser.value),
      registerAlias.value,
      registerPublicKey.value.split(",").map((num) => Number(num))
    );

    return false;
  };

  form.onsubmit = handleSubmit;
}

export default setupRegisterIdentityForm;
