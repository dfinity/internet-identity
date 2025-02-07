import { Connection } from "$src/utils/iiConnection";
import { html, render, TemplateResult } from "lit-html";
import { LEGACY_II_URL, OFFICIAL_II_URL } from "./config";
import { dummyFeatures } from "./features";

// Show a warning banner if the build is not "official". This happens if either the build
// is a flavored build, or if the origin is not the official II URL.
export const showWarningIfNecessary = async (
  connection: Connection
): Promise<void> => {
  const config = await connection.getConfig();
  const relatedOrigins = config.related_origins[0] ?? [];

  if (dummyFeatures()) {
    showWarning(html`Test only. Do not use your regular Internet Identity!
      <a
        class="features-warning-btn"
        target="_blank"
        rel="noopener noreferrer"
        href="https://github.com/dfinity/internet-identity#build-features"
        >more</a
      >`);
  } else if (
    // TODO: Remove these two hardcoded checks once related origins is set
    window.location.origin !== OFFICIAL_II_URL &&
    window.location.origin !== LEGACY_II_URL &&
    !relatedOrigins.includes(window.location.origin)
  ) {
    showWarning(html`This is not the official Internet Identity.
      <a
        class="features-warning-btn"
        target="_blank"
        rel="noopener noreferrer"
        href=${OFFICIAL_II_URL}
        >go to official</a
      >`);
  }
};

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
        position: fixed;
        top: 0;
        left: 0;
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
  document.body.append(container);
  return container;
};
