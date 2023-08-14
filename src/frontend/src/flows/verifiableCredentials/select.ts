import { hackerIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { KnownDapp } from "$src/flows/dappsExplorer/dapps";
import { I18n } from "$src/i18n";
import { IIWebAuthnIdentity } from "$src/utils/iiConnection";
import { mount, renderPage } from "$src/utils/lit-html";
import { NonEmptyArray } from "$src/utils/utils";
import { html, TemplateResult } from "lit-html";
import { vcStepper } from "./stepper";

/* Anchor construction component (for creating WebAuthn credentials) */

const selectTemplate = ({
  i18n,
  userNumber,
  relying,
  providers,
  scrollToTop = false,
}: {
  i18n: I18n;
  userNumber: bigint;
  relying: { dapp: KnownDapp; reason: string };
  providers: NonEmptyArray<KnownDapp>;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const slot = html`
    ${vcStepper({ current: "select" })}
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">${userNumber},</h1>
      <div class="l-stack hacker-wrap">${hackerIcon}</div>
      <p class="t-paragraph">
        select the service that you want to certify that ${relying.reason}.
        ${relying.dapp.name} will know which service you choose.
      </p>

      <ul class="l-stack">
        ${providers.map(
          (provider) => html`
            <li style="display: flex; gap: 0.5em; align-items: center;">
              <img
                style="width: 2em; aspect-ratio: 1;"
                src=${provider.logoSrc}
              /><span style="flex-grow: 1">${provider.name}</span>
              <button style="width: 5em;" class="c-button">select</button>
            </li>
          `
        )}
      </ul>
    </hgroup>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const selectPage = renderPage(selectTemplate);

// Prompt the user to create a WebAuthn identity
export const select = ({
  userNumber,
  relying,
  providers,
}: {
  userNumber: bigint;
  relying: { dapp: KnownDapp; reason: string };
  providers: NonEmptyArray<KnownDapp>;
}): Promise<IIWebAuthnIdentity | "canceled"> => {
  return new Promise((resolve) =>
    selectPage({
      i18n: new I18n(),
      userNumber,
      relying,
      providers,
      scrollToTop: true,
    })
  );
};
