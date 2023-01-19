import { html } from "lit-html";

export const compatibilityData = {
  note: `Internet Identity leverages the widely used secure web2 authentication
      framework known as WebAuthentication (WebAuthn). This way, you can use
      your fingerprint sensor, YubiKey or device passcode to store highly-secure
      secret keys on your devices and browsers without needing to create new
      passwords.`,

  note2: `However, WebAuthn is constantly evolving. Your preferred 
    browser may not support WebAuthentication, or may only support it using a 
    security key. If you run into issues, please try again with one of our 
    recommended browsers.`,

  desktop: [
    "Chrome version 67 or newer",
    "Firefox version 60 or newer",
    "Safari version 13 or newer",
    "Edge version 18 or newer",
  ],

  mobile: [
    "Chrome (Android)",
    "Safari 14.4 or newer (iOS)",
    "Firefox latest (iOS)",
    "Chrome latest (iOS)",
  ],
};

export const compatibilityChart = html` <p class="t-paragraph">
    ${compatibilityData.note}
  </p>
  <p class="t-paragraph">${compatibilityData.note2}</p>
  <h3 class="t-title">Recommended Desktop</h3>
  <ul class="c-list c-list--bulleted l-stack">
    ${compatibilityData.desktop.map((browser) => html`<li>${browser}</li>`)}
  </ul>
  <h3 class="t-title">Recommended Mobile</h3>
  <ul class="c-list c-list--bulleted l-stack">
    ${compatibilityData.mobile.map((browser) => html`<li>${browser}</li>`)}
  </ul>`;
