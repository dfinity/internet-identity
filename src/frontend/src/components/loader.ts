import { ERROR_SUPPORT_URL } from "$src/config";
import { html, render } from "lit-html";

// Duration in milliseconds a user considers as taking forever
const TAKING_FOREVER = 10000;

const loader = (takingForever = false) =>
  html` <div id="loader" class="c-loader">
    <div class="c-loader__spinner" />
    ${takingForever
      ? html`<a
          href="${ERROR_SUPPORT_URL}"
          target="_blank"
          rel="noopener noreferrer"
          class="c-loader__link"
          >Check ongoing issues</a
        >`
      : ""}
  </div>`;

const startLoader = (showCheckOngoingIssues?: boolean) => {
  const container = document.getElementById("loaderContainer") as HTMLElement;
  render(loader(showCheckOngoingIssues), container);

  const takingForeverTimeout = setTimeout(
    () => render(loader(true), container),
    TAKING_FOREVER
  );

  return () => {
    clearTimeout(takingForeverTimeout);
    render(html``, container);
  };
};

export const withLoader = async <A>(
  action: () => Promise<A>,
  showCheckOngoingIssues?: boolean
): Promise<A> => {
  const endLoader = startLoader(showCheckOngoingIssues);
  try {
    return await action();
  } finally {
    endLoader();
  }
};
