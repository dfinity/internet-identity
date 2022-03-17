// Contains code related to the "build features". Features should be accessed
// from the `features` object below. This file also contains helper functions
// for displaying a banner if features are enabled.
import { render, html } from "lit-html";

export const features = {
  FETCH_ROOT_KEY: process.env.II_FETCH_ROOT_KEY === "1",
  DUMMY_AUTH: process.env.II_DUMMY_AUTH === "1",
  DUMMY_CAPTCHA: process.env.II_DUMMY_CAPTCHA === "1",
};

export const anyFeatures = (): boolean => {
  return Object.values(features).indexOf(true) >= 0;
};

export const showWarningOnFeatures = (): void => {
  if (anyFeatures()) {
    showWarning();
  }
};

export const showWarning = (): void => {
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
        margin: 0 0 1rem;
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
    This is an insecure development version of Internet Identity.
    <a
      class="features-warning-btn"
      target="_blank"
      rel="noopener noreferrer"
      href="https://github.com/dfinity/internet-identity#build-features"
      >more</a
    >
  `;

  render(warning, container);
  document.body.prepend(container);
};
