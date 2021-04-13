import * as React from "react";
import actor from "../utils/actor";

interface Props {}

function RemoveIdentity(props: Props) {
  const {} = props;

  const handleSubmit = (e: React.SyntheticEvent) => {
    e.preventDefault();
    const target = e.target as HTMLFormElement;
    const removeUser = target.querySelector("#removeUser") as HTMLInputElement;
    const removePublicKey = target.querySelector(
      "#removePublicKey"
    ) as HTMLInputElement;

    actor.remove(
      BigInt(removeUser.value),
      removePublicKey.value.split(",").map((num) => Number(num))
    );

    return false;
  };

  return (
    <form onSubmit={handleSubmit} data-testid="removeForm">
      <button type="submit" id="remove-identity">
        Remove Identity
      </button>
      <label htmlFor="removeUser">
        User (expects a number)
        <input type="text" name="removeUser" id="removeUser" required />
      </label>
      <label htmlFor="removePublicKey">
        Public Key (expects a comma-separated list of numbers)
        <input
          type="text"
          name="removePublicKey"
          id="removePublicKey"
          required
        />
      </label>
    </form>
  );
}

export default RemoveIdentity;
