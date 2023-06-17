import { DynamicKey } from "$src/i18n";
import { html, TemplateResult } from "lit-html";
import { KnownDapp } from "./dapps";

export const dappsHeader = ({
  dapps,
  clickable,
}: {
  dapps: KnownDapp[];
  clickable: boolean;
}): TemplateResult => html`
  <figure
    class="c-card__teaser c-marquee ${clickable
      ? "c-marquee--clickable"
      : undefined}"
  >
    ${marqueeList(dapps)}
  </figure>
`;

/** A teaser for the dapps explorer, acting as a button */
export const dappsTeaser = ({
  dapps,
  click,
  copy: { dapps_explorer, sign_into_dapps },
}: {
  dapps: KnownDapp[];
  click: () => void;
  copy: { dapps_explorer: DynamicKey; sign_into_dapps: DynamicKey };
}): TemplateResult => {
  return html`<article class="c-card c-card--narrow">
    <div class="c-card__label" role="presentation">
      <h3>${dapps_explorer}</h3>
    </div>
    <h2 class="t-title">${sign_into_dapps}</h2>
    <button
      class="c-click-area"
      @click="${() => click()}"
      aria-label=${sign_into_dapps}
    >
      ${dappsHeader({ dapps, clickable: true })}
    </button>
  </article>`;
};

const marqueeList = (dapps: KnownDapp[]): TemplateResult => {
  const itemsPerRow = 5;
  const totalRows = 4;

  const paddedDapps = repeatArray({
    arr: dapps,
    length: itemsPerRow * totalRows,
  });

  /**
   * rows = [
   *   [dapp0, dapp1, dapp2, dapp3, dapp4],
   *   [dapp5, dapp6, dapp7, dapp8, dapp9],
   *   ...,
   *   [dapp0, dapp1, dapp2]
   * ]
   */
  const rows = chunkArray({ arr: paddedDapps, chunkSize: itemsPerRow });

  // rows are duplicated to create the infinite scrolling effect
  return html`<div
    aria-hidden="true"
    class="c-marquee__list"
    style="--itemsPerRow: ${itemsPerRow}; --totalRows: ${totalRows}"
  >
    ${rows.map((rowDapps, i) => {
      const rowContent = rowDapps.map(
        // XXX: it's important not to lazy load the image, otherwise
        // images start appearing before they're loaded. This then
        // shows an empty space where the image suddenly pops seconds
        // later.
        ({ logoSrc, name }) => html`<div class="c-marquee__item">
          <img src=${logoSrc} alt="${name}" class="c-marquee__image" />
        </div>`
      );

      return html`<div class="c-marquee__row" style="--rowIndex: ${i}">
        <div class="c-marquee__rowHalf">${rowContent}</div>
        <div class="c-marquee__rowHalf c-marquee__rowHalf--second">
          ${rowContent}
        </div>
      </div>`;
    })}
  </div>`;
};

// Helpers

/**
 * Pad the array by repeating it until it has the desired number of items.
 */
const repeatArray = <T>({ arr, length }: { arr: T[]; length: number }): T[] =>
  Array.from(new Array(length), (_, index) => arr[index % arr.length]);

/**
 * Chunk an array into chunks of the specified size (last chunk may be smaller)
 */
const chunkArray = <T>({
  arr,
  chunkSize,
}: {
  arr: T[];
  chunkSize: number;
}): T[][] =>
  Array.from(new Array(Math.ceil(arr.length / chunkSize)), (_, index) =>
    arr.slice(index * chunkSize, index * chunkSize + chunkSize)
  );
