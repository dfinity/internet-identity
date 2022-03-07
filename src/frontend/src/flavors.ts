// Contains code related to the "build flavors". Flavors should be accessed
// from the `flavors` object below. This file also contains helper functions
// for displaying a banner if flavors are enabled.
import { render, html } from "lit-html";

export const flavors = {
  FETCH_ROOT_KEY: process.env.II_FETCH_ROOT_KEY === "1",
  DUMMY_AUTH: process.env.II_DUMMY_AUTH === "1",
  DUMMY_CAPTCHA: process.env.II_DUMMY_CAPTCHA === "1",
};

export const anyFlavors = (): boolean => {
  return Object.values(flavors).indexOf(true) >= 0;
};

export const showWarningOnFlavors = (): void => {
  if (anyFlavors()) {
    showWarning();
  }
};

export const showWarning = (): void => {
  const container = document.createElement("div");
  container.className = "flavors-warning-container";
  const razzmatazz = "#ED1E79";
  const white = "#FFFFFF";

  const warning = html`
    <style>
      .flavors-warning-container {
        background: ${razzmatazz};
        color: ${white};
        width: 100vw;
        padding: 0.5em 1em;
        margin: 0 0 1rem;
        text-align: center;
      }

      .flavors-warning-btn {
        display: inline-block;
        margin-left: 1em;
        border-radius: 4px;
        border: ${white} solid 1px;
        padding: 0.2em 0.4em;
        text-decoration: none;
      }

      .flavors-warning-btn:link,
      .flavors-warning-btn:visited {
        color: ${white};
      }

      .flavors-warning-btn:hover,
      .flavors-warning-btn:focus {
        background: ${white};
        color: ${razzmatazz};
      }
    </style>
    This is an insecure development version of Internet Identity.
    <a
      class="flavors-warning-btn"
      target="_blank"
      rel="noopener noreferrer"
      href="https://github.com/dfinity/internet-identity#build-flavors"
      >more</a
    >
  `;

  render(warning, container);
  document.body.prepend(container);
};
