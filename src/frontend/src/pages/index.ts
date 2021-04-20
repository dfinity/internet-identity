import { render, html } from "lit-html";
import { initExistingUser } from "../flows/existingUser";
import { initNewUser } from "../flows/newUser";

const pageContent = () => html`<h1>Internet Identity</h1>
  <div class="spacer"></div>
  <h3>Are you an existing user?</h3>
  <button type="button" id="dialogTrigger" aria-controls="loginDialog">
    Log In
  </button>
  <div class="spacer"></div>
  <h3>If this is your first time using the Internet Identity</h3>
  <form id="registerForm">
    <label for="registerAlias">What should we call this device? </label>
    <input
      type="text"
      name="registerAlias"
      id="registerAlias"
      required
      placeholder="device name"
    />
    <button type="submit" id="register-identity">Click here to register</button>
  </form>
  <web-dialog id="loginDialog">
    <button type="button" aria-label="close dialog" id="closeDialog">
      <svg
        id="icon_close"
        data-name="icon/close"
        xmlns="http://www.w3.org/2000/svg"
        width="14"
        height="14"
        viewBox="0 0 14 14"
      >
        <rect
          id="Rectangle_91"
          data-name="Rectangle 91"
          width="14"
          height="14"
          fill="none"
        />
        <path
          id="Line"
          d="M0,0,9,9"
          transform="translate(2.5 2.5)"
          fill="none"
          stroke="gray"
          stroke-miterlimit="10"
          stroke-width="3"
        />
        <path
          id="Line-2"
          data-name="Line"
          d="M9,0,0,9"
          transform="translate(2.5 2.5)"
          fill="none"
          stroke="gray"
          stroke-miterlimit="10"
          stroke-width="3"
        />
      </svg>
    </button>
    <section>
      <h2>Existing User</h2>
    </section>
    <section id="userIdSection">
      <p>We couldn't automatically log you in. Enter your saved User #</p>
      <fieldset>
        <label for="registerUserNumber">User #</label>
        <input type="text" id="registerUserNumber" name="registerUserNumber" />
      </fieldset>
    </section>
    <section>
      <p>
        If you have authenticated using this device before, you can attempt to
        re-authenticate by clicking Reconnect. Otherwise, click Link to set this
        device up using an already authenticated browser
      </p>
      <div class="spacer"></div>
      <div class="row">
        <button id="reconnectTrigger">Reconnect</button>
        <button id="toggleAddDevice" aria-controls="addDeviceLinkSection">
          Link
        </button>
      </div>
    </section>
    <section class="hidden" id="addDeviceLinkSection" aria-expanded="false">
      <p>
        Copy the URL below and open it in your already authenticated browser.
        This page will automatically update when you have succeeded.
      </p>
      <input type="text" id="addDeviceLink" readonly />
    </section>
  </web-dialog>
  <div id="notification"></div>`;

export const renderIndex = () => {
  const container = document.getElementById("pageContent") as HTMLElement;

  render(pageContent(), container);
  initExistingUser();
  initNewUser();
};
