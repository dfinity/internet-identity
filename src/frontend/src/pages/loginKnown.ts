import { render, html } from "lit-html";
import { IDPActor } from "../utils/idp_actor";
import { getUserId } from "../utils/userId";
import { afterLogin } from "./afterLogin";
import { loginUnknown } from "./loginUnknown";

const pageContent = (userId: bigint) => html`
    <h1>Welcome back ${userId}</h1>
    <button type="button" id="login">Login</button>
    <button type="button" id="loginDifferent">Login as a different User</button>
`;

export const loginKnown = () => {
    const userId = getUserId();
    if (userId === undefined) {
        loginUnknown()
    } else {
        const container = document.getElementById("pageContent") as HTMLElement;
        render(pageContent(userId), container);
        init(userId)
    }
};

const init = (userId: bigint) => {
    const loginButton = document.querySelector(
      "#login"
    ) as HTMLButtonElement;
    const loginDifferentButton = document.querySelector(
      "#loginDifferent"
    ) as HTMLButtonElement;

    loginButton.onclick = async (ev) => {
        ev.preventDefault();
        await IDPActor.reconnect(userId).then(connection => {
            afterLogin(userId, connection)
        });
    }
    
    loginDifferentButton.onclick = (ev) => {
        ev.preventDefault();
        loginUnknown();
    };
}
