import { hackerIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { innerSpinnerTemplate } from "$src/components/spinner";
import { KnownDapp } from "$src/flows/dappsExplorer/dapps";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import { Chan, NonEmptyArray } from "$src/utils/utils";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { vcStepper } from "./stepper";

/* Anchor construction component (for creating WebAuthn credentials) */

const selectTemplate = <T>({
  i18n,
  userNumber,
  relying,
  providers,
  verify,
  onContinue,
  scrollToTop = false,
}: {
  i18n: I18n;
  userNumber: bigint;
  relying: { dapp: KnownDapp; reason: string };
  providers: NonEmptyArray<KnownDapp>;
  verify: (dapp: KnownDapp) => Promise<T>;
  onContinue: (result: T) => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const verifying = new Chan<boolean>(false);
  const slot = html`
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
              <button
                style="width: 5em;"
                class="c-button"
                @click=${async () => {
                  verifying.send(true);
                  let result: { ok: false } | { ok: true; res: T } = {
                    ok: false,
                  };
                  try {
                    result = { res: await verify(provider), ok: true };
                  } finally {
                    verifying.send(false);
                  }

                  if (result.ok) {
                    onContinue(result.res);
                  }
                }}
              >
                select
              </button>
            </li>
          `
        )}
      </ul>
    </hgroup>
  `;

  const spinner = html`
    <div class="t-centered l-stack">
      ${innerSpinnerTemplate()}
      <p class="t-lead t-paragraph" style="margin-top: 6rem">
        Hang tight, getting verification
      </p>
    </div>
  `;

  const content = verifying.map((verifying) => (verifying ? spinner : slot));

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: html` ${vcStepper({ current: "select" })} ${asyncReplace(content)} `,
  });
};

export const selectPage = <T>(
  props: Parameters<typeof selectTemplate<T>>[0],
  container?: HTMLElement
) => renderPage(selectTemplate<T>)(props, container);

// Prompt the user to create a WebAuthn identity
export const select = <T>({
  userNumber,
  relying,
  providers,
  verify,
}: {
  userNumber: bigint;
  relying: { dapp: KnownDapp; reason: string };
  providers: NonEmptyArray<KnownDapp>;
  verify: (dapp: KnownDapp) => Promise<T>;
}): Promise<T> => {
  return new Promise<T>((resolve) =>
    selectPage<T>({
      i18n: new I18n(),
      userNumber,
      relying,
      providers,
      verify,
      onContinue: (result: T) => resolve(result),
      scrollToTop: true,
    })
  );
};
