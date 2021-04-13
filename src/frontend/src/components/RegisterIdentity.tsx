import * as React from "react";
import actor from "../utils/actor";

interface Props {}

function RegisterIdentity(props: Props) {
  const {} = props;

  const handleSubmit = (e: React.SyntheticEvent) => {
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

  return (
    <form onSubmit={handleSubmit} data-testid="registerForm">
      <h2>Register an identity</h2>
      <label htmlFor="registerUser">
        User (expects a number)
        <input type="text" name="registerUser" id="registerUser" required />
      </label>
      <label htmlFor="registerAlias">
        Alias (expects a string)
        <input type="text" name="registerAlias" id="registerAlias" required />
      </label>
      <label htmlFor="registerPublicKey">
        Public Key (expects a comma-separated list of numbers)
        <input
          type="text"
          name="registerPublicKey"
          id="registerPublicKey"
          required
        />
      </label>
      <button type="submit" id="register-identity">
        Register Identity
      </button>
    </form>
  );
}

export default RegisterIdentity;
