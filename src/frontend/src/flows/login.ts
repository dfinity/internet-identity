import { render, html } from "lit-html";
import { confirm } from "../components/confirm";
import { IDPActor } from "../utils/idp_actor";
import { getUserId } from "../utils/userId";
import { LoginResult, loginUnknown } from "./loginUnknown";

const pageContent = (userId: bigint) => html`
  <style>
    #login {
      margin-bottom: 3rem;
      font-size: 1.25rem;
    }
    p,
    span,
    button {
      font-size: 0.85rem;
      margin: auto;
    }
    @media (min-width: 1024px) {
      p,
      span,
      button {
        font-size: 0.85rem;
        margin: initial;
      }
    }
  </style>
  <section>
    <h1>Welcome back, user #${userId}</h1>
    <button type="button" id="login" class="primary">Login</button>
    <p>
      <span>Or, </span
      ><button type="button" class="linkStyle" id="loginDifferent">
        sign in with a different identity
      </button>
    </p>
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
        await confirm({ message: x.message, detail: x.detail });
        return login();
      }
    }
  } catch (err) {
    await confirm({
      message: "An unexpected error occured during login. Please try again",
      detail: err,
    });
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
