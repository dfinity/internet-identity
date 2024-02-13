import { authWithII } from "./auth";

const button = document.querySelector(
  "[data-button-id=authenticate]"
)! as HTMLButtonElement;
const isAuthed = document.querySelector(
  "[data-output-id=is-authed]"
)! as HTMLOutputElement;

button.addEventListener("click", async () => {
  const resp = await fetch("/challenge");
  const obj = await resp.json();
  const challenge = obj.challenge;
  const delegationIdentity = await authWithII({
    url: "http://internet_identity.localhost:5173",
    sessionPublicKey: Uint8Array.from(atob(challenge), (c) => c.charCodeAt(0)),
  });
  console.log(delegationIdentity);
  const data = { challenge, delegationIdentity };
  await fetch("/verify", {
    method: "POST",
    body: JSON.stringify(data, (_, v) =>
      typeof v === "bigint" ? v.toString() : v
    ),
  });
});

button.disabled = false;
