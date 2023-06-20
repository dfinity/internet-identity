import { infoIconNaked, warningIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { DynamicKey } from "$src/i18n";
import { mount, TemplateElement } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";

/* An entry in the info screen */
type Entry = {
  /* The entry header */
  header: TemplateElement;
  /* Either a single element (like a paragraph) or a list (will be wrapped in a
   * bulleted list) */
  content: TemplateElement | TemplateElement[];
};

/* A simple screen which includes:
 *  * simple header + paragraph, with icon (info/warning) and label
 *  * next/cancel buttons
 *  * extra information (list of entries)
 */
export const infoScreenTemplate = ({
  next,
  nextText,
  cancel,
  cancelText,
  title,
  paragraph,
  entries,
  pageId,
  label,
  icon,
  scrollToTop = false,
}: {
  next: () => void;
  nextText: DynamicKey | string;
  cancel: () => void;
  cancelText: DynamicKey | string;
  title: DynamicKey | string;
  paragraph: TemplateElement;
  entries: {
    header: TemplateElement;
    content: TemplateElement | TemplateElement[];
  }[];
  pageId: string;
  label: DynamicKey | string;
  icon: "info" | "warning";
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const slot = html`
    <hgroup
      data-page=${pageId}
      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
    >
      <div class="c-card__label c-card__label--hasIcon">
        ${{ info: infoLabelIcon, warning: warningLabelIcon }[icon]}
        <h2>${label}</h2>
      </div>
      <h1 class="t-title t-title--main">${title}</h1>
      <p class="t-paragraph">${paragraph}</p>
    </hgroup>
    <div class="l-stack">
      <button @click=${() => next()} data-action="next" class="c-button">
        ${nextText}
      </button>
      <button
        @click=${() => cancel()}
        data-action="cancel"
        class="c-button c-button--secondary"
      >
        ${cancelText}
      </button>
    </div>
    <section style="margin-top: 7em;" class="c-marketing-block">
      ${entries.map((entry) => renderEntry(entry))}
    </section>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

/// Helpers

const renderEntry = (entry: Entry): TemplateResult => {
  const renderBulletList = (content: TemplateElement[]) => html`
    <ul class="c-list c-list--bulleted">
      ${content.map((line) => html`<li>${line}</li>`)}
    </ul>
  `;

  const renderBlock = (content: TemplateElement) => html`
    <p class="t-paragraph">${content}</p>
  `;

  return html`
    <aside class="l-stack">
      <h3 class="t-title">${entry.header}</h3>
      ${Array.isArray(entry.content)
        ? renderBulletList(entry.content)
        : renderBlock(entry.content)}
    </aside>
  `;
};

const infoLabelIcon: TemplateResult = html`
  <i class="c-card__icon c-icon c-icon--info__flipped c-icon--inline"
    >${infoIconNaked}</i
  >
`;

const warningLabelIcon: TemplateResult = html`
  <i class="c-card__icon c-icon c-icon--error__flipped c-icon--inline"
    >${warningIcon}</i
  `;
