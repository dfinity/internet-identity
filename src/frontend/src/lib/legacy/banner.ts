import type { InternetIdentityInit } from "$lib/generated/internet_identity_types";
import { html, render, TemplateResult } from "lit-html";
import { OFFICIAL_II_URL } from "../config";
import { anyFeatures } from "./features";

// Show a warning banner if the build is not "official". This happens if either the build
// is a flavored build, or if the is_production flag is not set to true.
export const showWarningIfNecessary = (config: InternetIdentityInit): void => {
  const isProduction: boolean = config.is_production[0] ?? false;
  const firstUrl: string = config.related_origins[0]?.[0] ?? OFFICIAL_II_URL;
  if (anyFeatures()) {
    showWarning(
      html`Test only. Do not use your regular Internet Identity!
        <a
          class="features-warning-btn"
          target="_blank"
          rel="noopener noreferrer"
          href="https://github.com/dfinity/internet-identity#build-features"
          >more</a
        >`,
    );
  } else if (!isProduction) {
    showWarning(
      html`This is not the official Internet Identity.
        <a
          class="features-warning-btn"
          target="_blank"
          rel="noopener noreferrer"
          href=${firstUrl}
          >go to official</a
        >`,
    );
  }
};

// Exported to be used in the showcase
export const showWarning = (message: TemplateResult): HTMLDivElement => {
  const container = document.createElement("div");
  container.className = "features-warning-container";
  container.setAttribute("role", "alert");
  const razzmatazz = "#ED1E79";
  const white = "#FFFFFF";

  const warning = html`
    <style>
      .features-warning-container {
        background: ${razzmatazz};
        color: ${white};
        width: 100%;
        box-sizing: border-box;
        padding: 0.5em 1em;
        text-align: center;
      }

      .features-warning-btn {
        display: inline-block;
        margin-left: 1em;
        border-radius: 4px;
        border: ${white} solid 1px;
        padding: 0.2em 0.4em;
        text-decoration: none;
      }

      .features-warning-btn:link,
      .features-warning-btn:visited {
        color: ${white};
      }

      .features-warning-btn:hover,
      .features-warning-btn:focus {
        background: ${white};
        color: ${razzmatazz};
      }
    </style>
    ${message}
  `;

  render(warning, container);
  document.body.prepend(container);
  return container;
};
