import { authWithII } from "./auth";

const button = document.querySelector(
  "[data-button-id=authenticate]"
)! as HTMLButtonElement;
const isAuthed = document.querySelector(
  "[data-output-id=is-authed]"
)! as HTMLOutputElement;

export const uint8ArrayToHexString = (bytes: Uint8Array | number[]) => {
  if (!(bytes instanceof Uint8Array)) {
    bytes = Uint8Array.from(bytes);
  }
  return bytes.reduce(
    (str, byte) => str + byte.toString(16).padStart(2, "0"),
    ""
  );
};

button.addEventListener("click", async () => {
  const resp = await fetch("/challenge");
  const obj: { challenge: string } = await resp.json();
  const challenge = obj.challenge;
  const delegationIdentity = await authWithII({
    // The url needs to be aligned with the root key in the backend
    // url: "http://internet_identity.localhost:5173",
    url: "https://jqajs-xiaaa-aaaad-aab5q-cai.ic0.app/",
    sessionPublicKey: new Uint8Array(Buffer.from(challenge, "base64")),
  });
  const data = { challenge, delegationIdentity };
  await fetch("/verify", {
    method: "POST",
    body: JSON.stringify(data, (_, v) => {
      if (typeof v === "bigint") {
        // We need to expiration date to be hex string.
        return v.toString(16);
      }
      if (v instanceof Uint8Array) {
        // We need the keys to be hex strings.
        return uint8ArrayToHexString(v);
      }
      return v;
    }),
    headers: new Headers({
      "Content-Type": "application/json",
    }),
  });
});

button.disabled = false;
