import { compatibilityChart } from "$src/components/compatibilityChart";
import { mainWindow } from "$src/components/mainWindow";
import { warnBox } from "$src/components/warnBox";
import { html, render } from "lit-html";

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = (reason: string) => html`
  <h1 class="t-title t-title--main" id="compatibilityNotice">
    Internet Identity does not support your browser
  </h1>
  ${warnBox({
    title: "Reason",
    message: reason,
    additionalClasses: ["l-stack"],
  })}
  <div class="l-stack">${compatibilityChart}</div>
`;

export const compatibilityNotice = (reason: string): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(
    mainWindow({
      slot: pageContent(reason),
      showLogo: false,
    }),
    container
  );
};
