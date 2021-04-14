import actor from "../utils/actor";

function setupLookupIdentityForm() {
  const form = document.getElementById("lookupForm") as HTMLFormElement;

  const handleSubmit = async (e) => {
    e.preventDefault();
    const target = e.target as HTMLFormElement;
    const lookupUser = target.querySelector("#lookupUser") as HTMLInputElement;

    const returnedValue = await actor.lookup(BigInt(lookupUser.value));
    console.info(returnedValue);

    return false;
  };
  form.onsubmit = handleSubmit;
}

export default setupLookupIdentityForm;
