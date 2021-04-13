import * as React from "react";
import actor from "../utils/actor";

interface Props {}

function LookupIdentity(props: Props) {
  const {} = props;

  const handleSubmit = (e: React.SyntheticEvent) => {
    e.preventDefault();
    const target = e.target as HTMLFormElement;
    const lookupUser = target.querySelector("#lookupUser") as HTMLInputElement;
    const lookupAlias = target.querySelector(
      "#lookupAlias"
    ) as HTMLInputElement;
    const lookupPublicKey = target.querySelector(
      "#lookupPublicKey"
    ) as HTMLInputElement;

    actor.lookup(BigInt(lookupUser.value));

    return false;
  };

  return (
    <form onSubmit={handleSubmit} data-testid="lookupForm">
      <button type="submit" id="lookup-identity">
        Lookup Identity
      </button>
      <label htmlFor="lookupUser">
        User (expects a number)
        <input type="text" name="lookupUser" id="lookupUser" required />
      </label>
    </form>
  );
}

export default LookupIdentity;
