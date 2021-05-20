import { html } from "lit-html";

export const compatibilityChart = html`<p>
    Browser support for WebAuthentication is constantly evolving. Your preferred
    browser may not support WebAuthentication, or may only support it using a
    security key. If you run into issues, we please try again with one of our
    recommended browsers.
  </p>
  <h3>For Desktop</h3>
  <ul>
    <li>Chrome version 67 or newer</li>
    <li>Firefox version 60 or newer</li>
    <li>Safari version 13 or newer</li>
    <li>Edge version 18 or newer</li>
  </ul>
  <h3>For Mobile</h3>
  <ul>
    <li>Chrome (Android)</li>
    <li>Safari 14.4 or newer (iOS)</li>
    <li>Firefox latest (iOS)</li>
    <li>Chrome latest (iOS)</li>
  </ul>`;
