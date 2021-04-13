import * as React from "react";
import actor from "../utils/actor";

interface Props {}

function AddIdentity(props: Props) {
  const {} = props;

  const handleSubmit = (e: React.SyntheticEvent) => {
    e.preventDefault();
    const target = e.target as HTMLFormElement;
    const addUser = target.querySelector("#addUser") as HTMLInputElement;
    const addAlias = target.querySelector("#addAlias") as HTMLInputElement;
    const addPublicKey = target.querySelector(
      "#addPublicKey"
    ) as HTMLInputElement;

    actor.add(
      BigInt(addUser.value),
      addAlias.value,
      addPublicKey.value.split(",").map((num) => Number(num))
    );

    return false;
  };

  return (
    <form onSubmit={handleSubmit} data-testid="addForm">
      <button type="submit" id="add-identity">
        Add Identity
      </button>
      <label htmlFor="addUser">
        User (expects a number)
        <input type="text" name="addUser" id="addUser" required />
      </label>
      <label htmlFor="addAlias">
        Alias (expects a string)
        <input type="text" name="addAlias" id="addAlias" required />
      </label>
      <label htmlFor="addPublicKey">
        Public Key (expects a comma-separated list of numbers)
        <input type="text" name="addPublicKey" id="addPublicKey" required />
      </label>
    </form>
  );
}

export default AddIdentity;
