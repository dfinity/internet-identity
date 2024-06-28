import { html, TemplateResult } from "lit-html";
import { navigationLink } from "./footer";
import { githubIcon, icLogo, questionIcon } from "./icons";

/**
 * Landing page template
 *
 * It is a component with split panes left and right in desktop view.
 * To the left there is some static content, and to the right there is a slot.
 *
 * In movile view, the static content is below the slot.
 *
 */
export const landingPage = ({
  slot,
}: {
  slot: TemplateResult;
}): TemplateResult => {
  return html` <main class="c-landingPage">
    <div class="c-landingPage__logo">
      <div class="c-logo">${icLogo}</div>
    </div>
    <section class="c-landingPage__right">
      <div class="c-landingPage__right__content">
        <div class="c-landingPage__right__content--full-width">${slot}</div>
      </div>
    </section>
    <section class="c-landingPage__left">
      <div class="c-landingPage__left__content">
        <h1 class="t-title t-title--main">
          Secure, seamless & privacy-preserving digital identity
        </h1>
        <p class="t-paragraph">
          Internet Identity is a decentralized identity solution running
          end-to-end on the Internet Computer blockchain.
        </p>
      </div>
      <div class="c-landingPage__left__footer">
        ${navigationLink({
          icon: questionIcon,
          labelText: "Support",
          id: "support-link",
          url: "https://internetidentity.zendesk.com/hc/en-us",
          rel: "noopener noreferrer",
          classes: "t-link--discreet c-footer__link",
        })}
        ${navigationLink({
          icon: githubIcon,
          labelText: "Source code",
          id: "source-link",
          url: "https://github.com/dfinity/internet-identity",
          rel: "noopener noreferrer",
          classes: "t-link--discreet c-footer__link",
        })}
      </div>
    </section>
  </main>`;
};
