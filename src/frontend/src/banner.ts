import { render, html, TemplateResult } from "lit-html";
import { anyFeatures } from "./features";

// Show a warning banner if the build is not "official". This happens if either the build
// is a flavored build, or if the origin is not 'identity.ic0.app'.
export const showWarningIfNecessary = (): void => {
  const officialUrl = "https://identity.ic0.app";
  if (anyFeatures()) {
    showWarning(html`This is an insecure development version of Internet
      Identity.
      <a
        class="features-warning-btn"
        target="_blank"
        rel="noopener noreferrer"
        href="https://github.com/dfinity/internet-identity#build-features"
        >more</a
      >`);
  } else if (window.location.origin !== officialUrl) {
    showWarning(html`This is not the official Internet Identity.
      <a
        class="features-warning-btn"
        target="_blank"
        rel="noopener noreferrer"
        href=${officialUrl}
        >go to official</a
      >`);
  }
};

export const showWarning = (message: TemplateResult): void => {
  const container = document.createElement("div");
  container.className = "features-warning-container";
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
};
