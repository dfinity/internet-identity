import { html, render } from "lit-html";
import {
  IIConnection,
} from "../utils/iiConnection";
import { hasOwnProperty } from "../utils/utils";

// The FAQ page
const pageContent = html`
  <style>
    /* briefly flash the question when redirected to a particular question */
    @keyframes flash-question {
      0% {
        background-color: transparent;
      }
      50% {
        background-color: var(--rainbow-orange);
        border-radius: 0.3em;
      }
      100% {
        background-color: transparent;
      }
    }
    :target {
      animation-name: flash-question;
      animation-duration: 600ms;
    }
  </style>
  <main>
  <h1>This will show a captcha</h1>
  <img class="captcha">

  </main>
`;

// Open the anchor with id="foo" if the page hash is "#foo"
const openAnchor = (): void => {
  const hash = location.hash.substring(1);

  if (hash !== "") {
    const details = document.getElementById(hash);
    console.log(details);

    if (details) {
      details.setAttribute("open", "");
    }
  }
};

export const showCaptcha = (): Promise<void> => {
  document.title = "show captcha";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  openAnchor(); // needs to happen after DOM was rendered

  IIConnection.getCaptcha().then((e) => {
      console.log("Got captcha");
      if(hasOwnProperty(e, "png")) {
          console.log("captcha has png");
          const img = document.querySelector("img.captcha");
          if(img) {
              console.log("got img");
              img.setAttribute('src', `data:image/png;base64, ${e.png}`);
          }
      }
  }).catch((e) => {
      console.log("Failed to load captcha");
      console.log(e);
  });

  return new Promise((resolve) => {
      console.log("waiting");
  });
};
