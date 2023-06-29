import { html, TemplateResult } from "lit-html";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { footer } from "./footer";
import { icLogo } from "./icons";

import { getDapps } from "$src/flows/dappsExplorer/dapps";
import { dappsHeader } from "$src/flows/dappsExplorer/teaser";
import { shuffleArray } from "$src/utils/utils";

/** dapps visual used in sidebar */
const dappsVisual = (): TemplateResult => {
  const dapps = shuffleArray(getDapps());
  return dappsHeader({
    dapps,
    clickable: false,
  });
};

/**
 * main window template
 *
 * To avoid having to repeat the same structure in every page,
 * we use this component to wrap the content of each page.
 * This way, we can change the structure of the main window
 * in one place.
 *
 * It is a component that includes the logo, the footer, and the container.
 *
 */
export const mainWindowWithSidebar = ({
  slot,
  id,
  showFooter = true,
  showLogo = true,
  isWideContainer = false,
  additionalContainerClasses = [],
}: {
  slot: TemplateResult;
  id?: string;
  showFooter?: boolean;
  showLogo?: boolean;
  isWideContainer?: boolean;
  additionalContainerClasses?: string[];
  HTMLwrapperTag?: string;
}): TemplateResult => {
  const containerClasses = ["l-container"];
  if (isWideContainer === true) {
    containerClasses.push("l-container--wide");
  }
  if (additionalContainerClasses.length > 0) {
    containerClasses.push(...additionalContainerClasses);
  }
  // we do not use the logo from the icons file because it duplicates the the ID's
  // (also hides those corresponding IDs in the other instances of the logos when the sidebar is not visible)
  return html`
    <div class="l-wrap l-wrap--sidebar">
      <div class="l-sidebar is-hidden--mobile">
        <div class="l-sidebar__main">
          <div class="c-logo c-logo--sidebar">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 233 111"
          >
            <defs>
              <linearGradient
                id="grad-o-y-sidebar"
                x1="145.304"
                x2="221.385"
                y1="7.174"
                y2="85.958"
                gradientUnits="userSpaceOnUse"
              >
                <stop offset=".21" stop-color="#F15A24" />
                <stop offset=".684" stop-color="#FBB03B" />
              </linearGradient>
              <linearGradient
                id="grad-p-p-sidebar"
                x1="85.087"
                x2="9.006"
                y1="101.622"
                y2="22.838"
                gradientUnits="userSpaceOnUse"
              >
                <stop offset=".21" stop-color="#ED1E79" />
                <stop offset=".893" stop-color="#522785" />
              </linearGradient>
            </defs>
            <g transform="translate(0 2)">
              <path
                fill="url(#grad-o-y-sidebar)"
                d="M174.433 0c-12.879 0-26.919 6.6-41.758 19.6-7.04 6.159-13.12 12.759-17.679 18.038l.04.04v-.04s7.199 7.84 15.159 16.24c4.28-5.08 10.44-12 17.519-18.24 13.2-11.559 21.799-13.999 26.719-13.999 18.52 0 33.559 14.68 33.559 32.719 0 17.92-15.079 32.599-33.559 32.719-.84 0-1.92-.12-3.28-.4 5.4 2.32 11.2 4 16.72 4 33.918 0 40.558-22.12 40.998-23.72 1-4.04 1.52-8.28 1.52-12.64C230.391 24.4 205.272 0 174.433 0Z"
              />
              <path
                fill="url(#grad-p-p-sidebar)"
                d="M55.958 108.796c12.88 0 26.919-6.6 41.758-19.6 7.04-6.16 13.12-12.759 17.679-18.039l-.04-.04v.04s-7.199-7.84-15.159-16.24c-4.28 5.08-10.44 12-17.52 18.24-13.199 11.56-21.798 14-26.718 14-18.52-.04-33.559-14.72-33.559-32.76C22.4 36.48 37.48 21.8 55.958 21.68c.84 0 1.92.12 3.28.4-5.4-2.32-11.2-4-16.72-4C8.6 18.08 2 40.2 1.52 41.76A52.8 52.8 0 0 0 0 54.397c0 29.999 25.119 54.398 55.958 54.398Z"
              />
              <path
                fill="#29ABE2"
                d="M187.793 90.197c-17.36-.44-35.399-14.12-39.079-17.52-9.519-8.8-31.479-32.599-33.198-34.479C99.436 20.16 77.637 0 55.958 0h-.08C29.558.12 7.44 17.96 1.52 41.758c.44-1.56 9.12-24.119 40.958-23.319 17.36.44 35.479 14.32 39.199 17.72 9.52 8.8 31.479 32.598 33.199 34.478 16.079 18 37.878 38.159 59.557 38.159h.08c26.319-.12 48.478-17.96 54.358-41.759-.48 1.56-9.2 23.92-41.078 23.16Z"
              />
            <g>
          </svg>
          <h1 class="c-logo__type">Internet Identity</h1>
          </div>
          <h2 class="t-title t-title--main">
            Securely connect to dapps on the Internet Computer
          </h2>
        </div>
        <div class="l-sidebar__decoration">${dappsVisual()}</div>
      </div>
      <div
        id="${ifDefined(id !== null ? id : undefined)}"
        class="${containerClasses.join(" ")}"
      >

        ${
          showLogo
            ? html`<div class="c-logo is-hidden--desktop">${icLogo}</div>`
            : ""
        }
        <div class="c-card c-card--background">
          <div class="c-card c-card--highlight">${slot}</div>
        </div>
      </div>
      ${showFooter ? footer : ""}
    </div>
  `;
};
