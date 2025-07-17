import { ERROR_SUPPORT_URL } from "$lib/config";
import { html, render } from "lit-html";

// Use same import approach as in 'src/frontend/src/flows/dappsExplorer/dapps.ts'
// this makes the import the same format (url string) in both the build and showcase.
const loaderUrl = import.meta.glob("./loader.svg", {
  eager: true,
  query: "?url",
  import: "default",
})["./loader.svg"] as string;

// Duration in milliseconds a user considers as taking forever
const TAKING_FOREVER = 20000;

const loader = (takingForever = false) =>
  html`<div id="loader" class="c-loader">
    <img class="c-loader__image" src="${loaderUrl}" alt="loading" />
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
    TAKING_FOREVER,
  );

  return () => {
    clearTimeout(takingForeverTimeout);
    render(html``, container);
  };
};

export const withLoader = async <A>(
  action: () => Promise<A>,
  showCheckOngoingIssues?: boolean,
): Promise<A> => {
  const endLoader = startLoader(showCheckOngoingIssues);
  try {
    return await action();
  } finally {
    endLoader();
  }
};
