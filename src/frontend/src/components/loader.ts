import { ERROR_SUPPORT_URL } from "$src/config";
import { html, render } from "lit-html";
import loaderUrl from "./loader.svg";

// Duration in milliseconds a user considers as taking forever
const TAKING_FOREVER = 10000;

const loader = (takingForever = false) =>
  html` <div id="loader" class="c-loader">
    <img class="c-loader__image" src=${loaderUrl} alt="loading" />
    ${takingForever &&
    html`<a
      href="${ERROR_SUPPORT_URL}"
      target="_blank"
      rel="noopener noreferrer"
      class="c-loader__link"
      >Ongoing issues</a
    >`}
  </div>`;

const startLoader = () => {
  const container = document.getElementById("loaderContainer") as HTMLElement;
  render(loader(), container);

  const takingForeverTimeout = setTimeout(
    () => render(loader(true), container),
    TAKING_FOREVER
  );

  return () => {
    clearTimeout(takingForeverTimeout);
    render(html``, container);
  };
};

export const withLoader = async <A>(action: () => Promise<A>): Promise<A> => {
  const endLoader = startLoader();
  try {
    return await action();
  } finally {
    endLoader();
  }
};
