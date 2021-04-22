import { html, render } from "lit-html";

const loader = () => html`<style>
    #loader {
      position: fixed;
      top: 0;
      left: 0;
      width: 100vw;
      height: 100vh;
      background: rgba(0, 0, 0, 0.75);
    }
    #loader img {
      width: auto;
      min-width: auto;
      max-width: calc(100vw - 1rem);
    }
    @media (min-width: 767px) {
      #loader img {
        width: 600px;
      }
    }
    @media (min-width: 1024px) {
      #loader img {
        width: 800px;
      }
    }
  </style>
  <picture id="loader" class="flex center">
    <img src="/glitch-loop.webp" alt="loading" />
  </picture>`;

export const startLoader = () => {
  const container = document.getElementById("loaderContainer") as HTMLElement;
  render(loader(), container);
};

export const endLoader = () => {
  const container = document.getElementById("loaderContainer") as HTMLElement;
  render(html``, container);
};

export const withLoader = async <A>(action: () => Promise<A>): Promise<A> => {
  startLoader();
  try {
    return await action()
  } finally {
    endLoader()
  }
}
