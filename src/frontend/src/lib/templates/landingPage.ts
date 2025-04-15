import { I18n } from "$lib/legacy/i18n";
import { html, TemplateResult } from "lit-html";
import { navigationLink } from "./footer";
import { githubIcon, icLogo, questionIcon } from "./icons";
import copyJson from "./landingPage.json";

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
  dataPage,
  landingType,
}: {
  slot: TemplateResult;
  dataPage: string;
  landingType?: "firstTime" | "useExisting" | "pick";
}): TemplateResult => {
  const i18n = new I18n();
  const copy = i18n.i18n(copyJson);
  const isLanding = window.location.hash.length === 0;

  return html`<main class="c-landingPage" data-page="${dataPage}">
    <div class="c-landingPage__container">
      <div class="c-landingPage__logo">
        <div class="c-logo">${icLogo}</div>
      </div>
      ${isLanding && landingType !== "firstTime"
        ? html`<div class="c-landingPage__right__info">
            <h1>${copy.landing_title_1}</h1>
            <h2>${copy.landing_subtitle}</h2>
          </div>`
        : ""}
      <section class="c-landingPage__right" aria-label="Marketing Copy">
        <div class="c-landingPage__right__content">
          <div class="c-landingPage__right__content--full-width">${slot}</div>
        </div>
      </section>
      <section class="c-landingPage__left" aria-label="Action Pane">
        <div class="c-landingPage__left__content">
          <h1
            class="${isLanding
              ? "c-landingPage__title"
              : "c-landingPage__title__authorize"}"
          >
            <span>${isLanding ? copy.landing_title_1 : copy.title_1}</span>
            <span>${isLanding ? copy.landing_title_2 : copy.title_2}</span>
          </h1>
          <p class="t-paragraph">
            ${isLanding ? copy.landing_subtitle : copy.subtitle}
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
    </div>
  </main>`;
};
