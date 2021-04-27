import { html, render } from "lit-html";
import ClipboardJS from "clipboard";
import { checkmarkIcon } from "../components/icons";

const pageContent = (link: string) => html`
  <style>
      .linkBox {
          display: flex;
      }
      #linkText {
          margin-right: 1rem;
          width: 80%;
      }
      #linkCopy {
        width: 20%;
      }
  </style>
  <div class="container">
    <h1>New device</h1>
    <p>Copy the URL below and open it in your already authenticated browser. This page will automatically update when you have succeeded.</p>
    <label>URL</label>
    <div class="linkBox">
      <input id="linkText" value="${link}" readonly>
      <button id="linkCopy" data-clipboard-target="#linkText">Copy</button>
    </div>
  </div>
  `;

export const displayAddDeviceLink = (link: string) => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(link), container);
  init()
}

const init = () => {
  const linkCopy = document.getElementById("linkCopy") as HTMLButtonElement;
  new ClipboardJS(linkCopy).on('success', () => {
    const linkCopy = document.getElementById("linkCopy") as HTMLButtonElement;
    render(checkmarkIcon, linkCopy)
  })
};
