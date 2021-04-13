import * as React from "react";
import actor from "../utils/actor";

interface Props {}

function LookupIdentity(props: Props) {
  const {} = props;

  const handleSubmit = async (e: React.SyntheticEvent) => {
    e.preventDefault();
    const target = e.target as HTMLFormElement;
    const lookupUser = target.querySelector("#lookupUser") as HTMLInputElement;
    const lookupAlias = target.querySelector(
      "#lookupAlias"
    ) as HTMLInputElement;
    const lookupPublicKey = target.querySelector(
      "#lookupPublicKey"
    ) as HTMLInputElement;

    const returnedValue = await actor.lookup(BigInt(lookupUser.value));
    console.info(returnedValue);

    return false;
  };

  return (
    <form onSubmit={handleSubmit} data-testid="lookupForm">
      <h2>Lookup Identity</h2>
      <label htmlFor="lookupUser">
        User (expects a number)
        <input type="text" name="lookupUser" id="lookupUser" required />
      </label>
      <button type="submit" id="lookup-identity">
        Lookup Identity
      </button>
    </form>
  );
}

export default LookupIdentity;
