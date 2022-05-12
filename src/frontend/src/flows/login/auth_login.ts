import { html, render } from "lit-html";
import { icLogo } from "../../components/icons";
import { initLogout, logoutSection } from "../../components/logout";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";

const pageContent = (hostName: string, authEndTimestamp: number, userNumber?: bigint) => html` <style>
    .spacer {
      height: 2rem;
    }
    .userNumberInput {
      text-align: center;
      font-size: 1.5rem;
      font-weight: 500;
    }
    .userNumberInput:focus {
      box-sizing: border-box;
      border-style: double;
      border-width: 2px;
      border-radius: 4px;
      border-image-slice: 1;
      outline: none;
      border-image-source: linear-gradient(
        270.05deg,
        #29abe2 10.78%,
        #522785 22.2%,
        #ed1e79 42.46%,
        #f15a24 59.41%,
        #fbb03b 77.09%
      );
    }
    .smallText {
      font-size: 0.875rem;
      font-weight: 400;
    }
    .bold {
      font-weight: 600;
    }
  </style>
  <div class="container">
    ${icLogo}
    <h1>Authorize Authentication</h1>
    <p>Proceed to authenticate with</p>
    <div class="smallText highlightBox">${hostName}</div>
    <p class="bold">Identity Anchor</p>
    <input class="userNumberInput" value="${userNumber}" placeholder="Enter Identity Anchor">
    <button type="button" id="login" class="primary">Authenticate</button>
    <button type="button" id="login">Cancel</button>
    <span class="smallText">Max session validity: ${new Date(authEndTimestamp).toLocaleString()}</span>
    <div class="spacer"></div>
    <div class="textLink">
      Lost access
      <button id="recoverButton" class="linkStyle">and want to recover?</button>
    </div>
    ${logoutSection("Cancel and clear browser storage")} ${navbar}
  </div>
  ${footer}`;

export const authDapp = async (): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent('https://h5aet-waaaa-aaaab-qaamq-cai.ic0.app/', Date.now(), BigInt(1234156)), container);
  initLogout();
  return init();
};

const init = async () => {
  console.log('init');
}