import { render, html } from "lit-html";
import { initManageIdentities } from "../flows/manageIdentities";

const pageContent = () => html`<section id="intro">
    <h1>Identity Management</h1>
    <h3>Welcome <span id="nameSpan"></span></h3>
    <p>You can view and manage your Internet Computer identities here.</p>
  </section>
  <section id="userIdSection" class="hidden">
    <h3>Your user id is <span id="userIdSpan"></span></h3>
  </section>
  <section id="identityList"></section>
  <web-dialog id="prompt">
    <form action="" id="prompt-form">
      <p id="prompt-text"></p>
      <p class="details"></p>
      <input type="text" id="prompt-input" />
      <div class="flex row">
        <button type="submit">Confirm</button>
        <button type="button" id="prompt-cancel">Cancel</button>
      </div>
    </form>
  </web-dialog>
  <web-dialog id="confirm">
    <form action="" id="confirm-form">
      <p id="confirm-text"></p>
      <p class="details"></p>
      <div class="flex row">
        <button type="submit">Confirm</button>
        <button type="button" id="confirm-cancel">Cancel</button>
      </div>
    </form>
  </web-dialog>
  <div id="notification"></div>`;

export const renderManage = () => {
  const container = document.getElementById("pageContent") as HTMLElement;

  render(pageContent(), container);
  initManageIdentities();
};
