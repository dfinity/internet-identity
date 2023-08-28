import { badChallenge, promptCaptchaPage } from "$src/flows/register/captcha";
import { TemplateResult, html, render } from "lit-html";
import { dummyChallenge, i18n } from "./showcase";

export const flowsPage = () => {
  document.title = "Flows";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};

export const iiFlows: Record<string, () => void> = {
  captcha: () => {
    promptCaptchaPage({
      cancel: () => console.log("canceled"),
      requestChallenge: () =>
        new Promise((resolve) => setTimeout(resolve, 1000)).then(
          () => dummyChallenge
        ),
      verifyChallengeChars: (cr) =>
        new Promise((resolve) => setTimeout(resolve, 1000)).then(() =>
          cr.chars === "8wJ6Q" ? "yes" : badChallenge
        ),
      onContinue: () => console.log("Done"),
      i18n,
      focus: false,
    });
  },
};

const pageContent: TemplateResult = html`
  <div class="l-wrap">
    <div class="l-container">
      <div class="c-card c-card--background">
        <h1 class="t-title t-title--main">Flows</h1>
        <div class="l-stack">
          ${Object.entries(iiFlows).map(([flowName, _]) => {
            // '/' or '/internet-identity/'
            const baseUrl = import.meta.env.BASE_URL ?? "/";
            // '/myFlow' or '/internet-identity/myFlow'
            const flowLink = baseUrl + "flows/" + flowName;
            return html`<aside>
              <a data-page-name=${flowName} href=${flowLink}>
                <h2>${flowName}</h2>
              </a>
            </aside>`;
          })}
        </div>
      </div>
    </div>
  </div>
`;
