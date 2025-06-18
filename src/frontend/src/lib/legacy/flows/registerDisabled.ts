import { mainWindow } from "$lib/templates/mainWindow";
import { warnBox } from "$lib/templates/warnBox";
import { html, render } from "lit-html";

const pageContent = ({
  onCancel,
  allowedOrigins,
}: {
  onCancel: () => void;
  allowedOrigins: string[];
}) => {
  const pageContentSlot = html`<hgroup>
      <h1 class="t-title t-title--main">Registration Disabled</h1>
    </hgroup>
    <div class="l-stack">
      ${warnBox({
        title: "Registration Disabled",
        message: html`<p class="t-paragraph t-lead">
            To keep you safe, we disabled registration from this address. If you
            want to securely create a new Internet Identity, visit:
            ${allowedOrigins.length === 1
              ? html`<a class="t-link" href=${allowedOrigins[0]}
                    >${allowedOrigins[0]}</a
                  >.`
              : html`
                  <ul>
                    ${allowedOrigins.map(
                      (origin) =>
                        html`<li>
                          <a class="t-link" href=${origin}>${origin}</a>
                        </li>`,
                    )}
                  </ul>
                `}
          </p>
          <p class="t-paragraph">
            If you were redirected here by another website, please inform the
            developers. Read more about this security measure
            <a
              class="t-link"
              href="https://forum.dfinity.org/t/internet-identity-proposal-to-deprecate-account-creation-on-all-origins-other-than-https-identity-ic0-app/9760"
              >here</a
            >.
          </p>`,
      })}
    </div>
    <div class="l-stack">
      <button class="c-button" @click="${onCancel}" id="deviceAliasCancel">
        Cancel
      </button>
    </div>`;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: pageContentSlot,
  });
};

/**
 * Shows the register disabled page with a list of allowed origins
 * @param allowedOrigins Optional list of allowed origins
 * @returns Promise resolved when the user clicks cancel
 */
export const registerDisabled = (
  allowedOrigins: string[],
): Promise<{ tag: "canceled" }> => {
  return new Promise((resolve) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(
      pageContent({
        onCancel: () => {
          resolve({ tag: "canceled" });
        },
        allowedOrigins,
      }),
      container,
    );
  });
};
