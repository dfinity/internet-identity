import { html, TemplateResult } from "lit-html";

export type DappDescription = {
  id: string;
  name: string;
  oneLiner?: string;
  link: string;
  derivationOrigin?: string;
  description: string;
  logo: string;
};

const marqueeListRow = (dapps: DappDescription[]): TemplateResult => {
  return html` ${dapps.map(
    ({ logo, name }) => html`<div class="c-marquee__item">
      <img
        src="icons/${logo}"
        alt="${name}"
        class="c-marquee__image"
        loading="lazy"
      />
    </div>`
  )}`;
};

/**
 * The marquee is a list of dapps that scrolls horizontally.
 * It uses a CSS keyframe animation to scroll the each row alternating from left to right and viceversa.
 * To give the illusion of an infinite scrolling, the list is duplicated horizontally.
 * when the animation reaches the end of the list, it is reset to the beginning. (default keyframe animation behaviour)
 */

const marqueeList = (dapps: DappDescription[]): TemplateResult => {
  const itemsPerRow = 5;
  const totalRows = 4;

  /**
   * rows = [
   *   [dapp0, dapp1, dapp2, dapp3, dapp4],
   *   [dapp5, dapp6, dapp7, dapp8, dapp9],
   *   ...,
   *   [dapp0, dapp1, dapp2, dapp3, dapp4]
   * ]
   */
  const rows = Array.from(new Array(totalRows), (row, i) =>
    Array.from(
      new Array(itemsPerRow),
      (item, j) =>
        /**
         * If the number of dapps is less than the number of items per row, the list is repeated.
         * So if there are only 5 dapps but we want to show a grid of 5x5, the list is repeated 5 times.
         *
         * x = i * itemsPerRow
         * y = x + j
         * dappsIndex = x + y
         * dappsIndexWrapped = dappsIndex % dapps.length
         */
        dapps[(i * itemsPerRow + j) % dapps.length]
    )
  );

  // rows are duplicated to create the infinite scrolling effect
  return html`<div
    aria-hidden="true"
    class="c-marquee__list"
    style="--itemsPerRow: ${itemsPerRow}; --totalRows: ${totalRows}"
  >
    ${rows.map(
      (row, i) => html`<div class="c-marquee__row" style="--rowIndex: ${i}">
        <div class="c-marquee__rowHalf">${marqueeListRow(row)}</div>
        <div class="c-marquee__rowHalf c-marquee__rowHalf--second">
          ${marqueeListRow(row)}
        </div>
      </div>`
    )}
  </div>`;
};

export const dappsList = (dapps: DappDescription[]): TemplateResult => {
  return html` <article class="c-card c-card--narrow">
    <h2 class="t-title t-title--discrete">Dapps explorer</h2>
    <h2 class="t-title">Explore dapps</h2>
    <section aria-label="List of dapps">
      <div class="c-action-list">
        ${dapps.map(
          ({ link, logo, name, oneLiner }) => html`<a
            href="${link}"
            class="c-action-list__item"
            target="_blank"
            rel="noopener noreferrer"
          >
            <div class="c-action-list__icon" aria-hidden="true">
              <img src="icons/${logo}" alt="${name} logo" loading="lazy" />
            </div>
            <div class="c-action-list__label c-action-list__label--stacked">
              <h3 class="t-title t-title--list">${name}</h3>
              <p class="t-weak">${oneLiner}</p>
            </div>
            <span class="c-action-list__action"> ↗ </span>
          </a>`
        )}
      </div>
    </section>
  </article>`;
};

export const dappsTeaser = (
  dapps: DappDescription[],
  clickFn: () => void
): TemplateResult => {
  return html`<article class="c-card c-card--narrow">
    <h3 class="t-title t-title--discrete" role="presentation">
      Dapps explorer
    </h3>
    <h2 class="t-title">Explore dapps</h2>
    <button
      class="c-click-area"
      @click="${() => clickFn()}"
      aria-label="Show list of dapps"
    >
      <figure class="c-card__teaser c-marquee">${marqueeList(dapps)}</figure>
    </button>
  </article>`;
};
