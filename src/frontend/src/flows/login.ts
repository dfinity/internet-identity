import { render, html } from "lit-html";
import { confirm } from "../components/confirm";
import { IDPActor } from "../utils/idp_actor";
import { getUserId } from "../utils/userId";
import { LoginResult, loginUnknown } from "./loginUnknown";

const pageContent = (userId: bigint) => html`
  <section>
    <h1>Welcome back ${userId}</h1>
    <button type="button" id="login">Login</button>
    <button type="button" id="loginDifferent">Login as a different User</button>
  </section>
`;

// We retry logging in until we get a succesful user id connection pair
// If we encounter an unexpected error we reload to be safe
export const login = async (): Promise<{
  userId: bigint;
  connection: IDPActor;
}> => {
  try {
    const x = await tryLogin();

    switch (x.tag) {
      case "ok": {
        return { userId: x.userId, connection: x.connection };
      }
      case "err": {
        await confirm(x.message, x.detail);
        return login();
      }
    }
  } catch (err) {
    await confirm(
      "An unexpected error occured during login. Please try again",
      err
    );
    window.location.reload();
    return Promise.reject(err);
  }
};

const tryLogin = async (): Promise<LoginResult> => {
  const userId = getUserId();
  if (userId === undefined) {
    return loginUnknown();
  } else {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(userId), container);
    return init(userId);
  }
};

const init = async (userId: bigint): Promise<LoginResult> => {
  return new Promise((resolve) => {
    const loginButton = document.querySelector("#login") as HTMLButtonElement;
    const loginDifferentButton = document.querySelector(
      "#loginDifferent"
    ) as HTMLButtonElement;

    loginButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      try {
        resolve({
          tag: "ok",
          userId,
          connection: await IDPActor.reconnect(userId),
        });
      } catch (err) {
        resolve({
          tag: "err",
          message: `Failed to login as ${userId}`,
          detail: err.toString(),
        });
      }
    };

    loginDifferentButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      resolve(await loginUnknown());
    };
  });
};
