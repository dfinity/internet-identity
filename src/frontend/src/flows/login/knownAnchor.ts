import { render, html } from "lit-html";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { icLogo } from "../../components/icons";
import { withLoader } from "../../components/loader";
import { logoutSection, initLogout } from "../../components/logout";
import { Connection } from "../../utils/iiConnection";
import { loginUnknownAnchor } from "./unknownAnchor";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { useRecovery } from "../recovery/useRecovery";

const pageContent = (userNumber: bigint) => html`
  <div class="l-container c-card c-card--bg">
    <div class="c-card__bg">
      <canvas class="c-bgc" width="32" height="32"></canvas>
    </div>
    <div class="c-logo">${icLogo}</div>
    <hgroup class="l-section">
      <h1 class="t-title t-title--main">Welcome back!</h1>
      <p class="t-lead">Authenticate using Internet Identity.</p>
    <hgroup>
    <div class="l-section">
      <div class="c-animated-input">
        <output class="c-animated-input__input c-input c-input--readonly t-vip" aria-label="User Number" data-usernumber>${userNumber}</output>
        <button type="button" id="login" class="c-animated-input__button c-button">Authenticate</button>
        <canvas class="c-animated-input__bg" width="32" height="32"></canvas>
      </div>
      <div class="l-divider l-divider--text" aria-label="Other Options">
        <span class="l-divider__label" aria-hidden>Or</span>
      </div>
    </div>
    <button type="button" id="loginDifferent" class="c-button c-button--secondary">
      Use a different Identity Anchor
    </button>
    <div class="l-section">
      <p>
        Lost access
        <a id="recoverButton" class="t-link">and want to recover?</a>
      </p>
    </div>
    ${logoutSection("Clear Identity Anchor from browser")}
    ${navbar}
  </div>
  ${footer}`;

export const loginKnownAnchor = async (
  userNumber: bigint,
  connection: Connection
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init(userNumber, connection);
};

const init = async (
  userNumber: bigint,
  connection: Connection
): Promise<LoginFlowResult> => {
  return new Promise((resolve) => {
    startBgAnimation();
    initLogout();
    const loginButton = document.querySelector("#login") as HTMLButtonElement;
    const loginDifferentButton = document.querySelector(
      "#loginDifferent"
    ) as HTMLButtonElement;

    loginButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      const result = await withLoader(() => connection.login(userNumber));
      resolve(apiResultToLoginFlowResult(result));
    };

    loginDifferentButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      resolve(await loginUnknownAnchor(connection));
    };
    const recoverButton = document.getElementById(
      "recoverButton"
    ) as HTMLAnchorElement;
    recoverButton.onclick = () => useRecovery(connection, userNumber);
  });
};

const startBgAnimation = async () => {
  // const logoCanvas = document.querySelector(".can") as unknown as HTMLCanvasElement;
  const bgCanvas = document.querySelector(
    ".c-bgc"
  ) as unknown as HTMLCanvasElement;
  const inputCanvas = document.querySelector(
    ".c-animated-input__bg"
  ) as unknown as HTMLCanvasElement;
  const $svg = document.querySelector("svg") as unknown as HTMLElement;
  // const $ = logoCanvas.getContext("2d");
  const $bg = bgCanvas.getContext("2d") as CanvasRenderingContext2D;
  const $input = inputCanvas.getContext("2d") as CanvasRenderingContext2D;

  const drawBg = function (
    x: number,
    y: number,
    r: number,
    g: number,
    b: number
  ) {
    $bg.fillStyle = "rgb(" + r + "," + g + "," + b + ")";
    $bg.fillRect(x, y, 1, 1);
  };
  const R = function (x: number, y: number, t: number) {
    return Math.floor(192 + 64 * Math.cos((x * x - y * y) / 300 + t));
  };

  const G = function (x: number, y: number, t: number) {
    return Math.floor(
      192 +
        64 * Math.sin((x * x * Math.cos(t / 4) + y * y * Math.sin(t / 3)) / 300)
    );
  };

  const B = function (x: number, y: number, t: number) {
    return Math.floor(
      192 +
        64 *
          Math.sin(
            5 * Math.sin(t / 9) +
              ((x - 100) * (x - 100) + (y - 100) * (y - 100)) / 1100
          )
    );
  };

  let t = 0;

  const $img = document.createElement("img");
  const blob = new Blob([$svg.outerHTML], { type: "image/svg+xml" });
  const url = URL.createObjectURL(blob);
  // const image = document.createElement("img");
  $img.addEventListener(
    "load",
    () => {
      URL.revokeObjectURL(url);
      //run();
    },
    { once: true }
  );
  $img.src = url;

  const run = function () {
    for (let x = 0; x <= 35; x++) {
      for (let y = 0; y <= 35; y++) {
        drawBg(x, y, R(x, y, t), G(x, y, t), B(x, y, t));
      }
    }
    t = t + 0.05;
    // $bg.drawImage(logoCanvas, 0, 0);
    $input.drawImage(bgCanvas, 0, 0);
    window.requestAnimationFrame(run);
  };

  run();
};
