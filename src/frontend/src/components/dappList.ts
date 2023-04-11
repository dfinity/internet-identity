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

/**
 * The marquee is a list of dapps that scrolls horizontally.
 * It uses a CSS keyframe animation to scroll the each row alternating from left to right and viceversa.
 * To give the illusion of an infinite scrolling, the list is duplicated horizontally.
 * when the animation reaches the end of the list, it is reset to the beginning. (default keyframe animation behaviour)
 */

const createMarqueeList = (dapps: DappDescription[]): TemplateResult => {
  const itemsPerRow = 5;
  const totalRows = 4;

  const rows = [];

  for (let i = 0; i < totalRows; i++) {
    const row = [];
    for (let j = 0; j < itemsPerRow; j++) {
      const dapp = dapps[(i * itemsPerRow + j) % dapps.length];
      row.push(dapp);
    }
    rows.push([...row]);
  }

  // rows are duplicated to create the infinite scrolling effect
  return html`<div
    class="c-marquee__list"
    style="--itemsPerRow: ${itemsPerRow}; --totalRows: ${totalRows}"
  >
    ${rows.map(
      (row, i) => html`<div class="c-marquee__row" style="--rowEq: ${i}">
        <div class="c-marquee__rowHalf">
          ${row.map(
            (dapp) => html`<div class="c-marquee__item">
              <img
                src="icons/${dapp.logo}"
                alt="${dapp.name}"
                class="c-marquee__image"
                loading="lazy"
              />
            </div>`
          )}
        </div>
        <div class="c-marquee__rowHalf c-marquee__rowHalf--second">
          ${row.map(
            (dapp) => html`<div class="c-marquee__item">
              <img
                src="icons/${dapp.logo}"
                alt="${dapp.name}"
                class="c-marquee__image"
                loading="lazy"
              />
            </div>`
          )}
        </div>
      </div>`
    )}
  </div>`;
};

export const dappsListElement = (
  dappsList: DappDescription[]
): TemplateResult => {
  return html` <article class="c-card c-card--narrow">
    <h2 class="t-title t-title--discrete">Dapps explorer</h2>
    <h2 class="t-title">Explore dapps</h2>
    <section aria-label="List of dapps">
      <div class="c-action-list">
        ${dappsList.map(
          (dapp) => html` <a
            href="${dapp.link}"
            class="c-action-list__item"
            target="_blank"
            rel="noopener noreferrer"
          >
            <div class="c-action-list__icon" aria-hidden>
              <img
                src="icons/${dapp.logo}"
                alt="${dapp.name} logo"
                loading="lazy"
              />
            </div>
            <div class="c-action-list__label c-action-list__label--stacked">
              <h3 class="t-title t-title--list">${dapp.name}</h3>
              <p class="t-weak">${dapp.oneLiner}</p>
            </div>
            <span class="c-action-list__action"> ↗ </span>
          </a>`
        )}
      </div>
    </section>
  </article>`;
};

export const dappsTeaser = (dappsList: DappDescription[]): TemplateResult => {
  return html`<article class="c-card c-card--narrow">
    <h2 class="t-title t-title--discrete">Dapps explorer</h2>
    <h2 class="t-title">Explore dapps</h2>
    <figure class="c-card__teaser c-marquee">
      ${createMarqueeList(dappsList)}
    </figure>
  </article>`;
};
