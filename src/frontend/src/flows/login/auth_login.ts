import { html } from "lit-html";
import { icLogo } from "../../components/icons";
import { logoutSection } from "../../components/logout";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";

const pageContent = (hostName: string, authEndTimestamp: bigint, userNumber?: bigint) => html` <style>
    .spacer {
      height: 2rem;
    }
  </style>
  <div class="container">
    ${icLogo}
    <h1>Welcome back!</h1>
    <p>Authenticate using Internet Identity.</p>
    <div class="highlightBox">${userNumber}</div>
    <div id="confirmRedirectHostname" class="highlightBox">${hostName}</div>
    <button type="button" id="login" class="primary">Authenticate</button>
    <p style="text-align: center;">Or</p>
    <button type="button" id="loginDifferent">
      Use a different Identity Anchor
    </button>
    <div class="spacer"></div>
    <div class="textLink">
      Lost access
      <button id="recoverButton" class="linkStyle">and want to recover?</button>
    </div>
    ${logoutSection("Clear Identity Anchor from browser")} ${navbar}
  </div>
  ${footer}`;