import { html, render } from "lit-html";
import { hydrate } from "lit-html/experimental-hydrate.js";
import { compatibilityData } from "../../components/compatibilityChart";
import { mainWindow } from "../../components/mainWindow";
import { I18n } from "../../i18n";

import copyJson from "./copy.json" assert { type: "json" };

const aboutContentSlot = (i18n: I18n) => {
  const copy = i18n.i18n(copyJson);

  return html`
    <h1 class="t-title t-title--main">${copy.title}</h1>
    <div class="l-stack">
      <h2 class="t-title">${copy.header}</h2>
      <p class="t-lead">${copy.use_ii}</p>
    </div>
    <div class="l-stack">
      <h2 class="t-title">${copy.anchors_title}</h2>
      <p class="t-paragraph">
        ${copy.anchors}<br /><br /><a href="/">${copy.anchors_cta}</a>
      </p>
    </div>
    <div class="l-stack">
      <h2 class="t-title">${copy.privacy_convenience_title}</h2>
      <p class="t-paragraph">${copy.privacy_convenience}</p>
    </div>
    <div class="l-stack">
      <h2 class="t-title">${copy.explore_dapps}</h2>
      <p class="t-paragraph">${copy.ic_dapps}</p>
      <ul class="c-list c-list--bulleted l-stack">
        <li class="c-list__item">
          <a
            href=${copy.distrikt_link}
            target="_blank"
            rel="noopener noreferrer"
            >${copy.distrikt_name}</a
          >
          <p>${copy.distrikt_desc}</p>
        </li>
        <li class="c-list__item">
          <a
            href=${copy.discover_link}
            target="_blank"
            rel="noopener noreferrer"
            >${copy.discover_name}</a
          >
          <p>${copy.discover_desc}</p>
        </li>
        <li class="c-list__item">
          <a
            href=${copy.openchat_link}
            target="_blank"
            rel="noopener noreferrer"
          >
            ${copy.openchat_name}
          </a>
          <p>${copy.openchat_desc}</p>
        </li>
      </ul>
    </div>
    <div class="l-stack">
      <h2 class="t-title">Compatibility</h2>

      <div class="l-horizontal l-stack">
        <div class="about__compatibility-flexchild">
          <h3 class="t-title">Recommended Desktop</h3>
          <ul class="c-list c-list--bulleted l-stack">
            ${compatibilityData.desktop.map(
              (browser) => html`<li>${browser}</li>`
            )}
          </ul>
        </div>
        <div class="about__compatibility-flexchild">
          <h3 class="t-title">Recommended Mobile</h3>
          <ul class="c-list c-list--bulleted l-stack">
            ${compatibilityData.mobile.map(
              (browser) => html`<li>${browser}</li>`
            )}
          </ul>
        </div>
      </div>
      <p class="t-paragraph">${compatibilityData.note}</p>
      <p class="t-paragraph">${compatibilityData.note2}</p>
    </div>
  `;
};

// The About page
export const pageContent = (i18n: I18n) =>
  mainWindow({
    isWideContainer: true,
    slot: aboutContentSlot(i18n),
  });

export const aboutView = (): void => {
  document.title = "About | Internet Identity";
  const container = document.getElementById("pageContent") as HTMLElement;
  const i18n = new I18n();
  if (process.env.HYDRATE_STATIC_PAGES !== "0") {
    hydrate(pageContent(i18n), container);
  }

  render(pageContent(i18n), container);
};
