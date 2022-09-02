// TODO: implement destroy mechanism
export const startCardAnimation = () => {
  console.log("startCardAnimation");

  const bgCanvas: null | HTMLCanvasElement =
    document.querySelector(".c-card-bg__canvas");
  const inputCanvas: null | HTMLCanvasElement = document.querySelector(
    ".c-animated-input__bg"
  );
  // const $svg = document.querySelector("svg") as unknown as HTMLElement;
  const $bg =
    bgCanvas && (bgCanvas.getContext("2d") as CanvasRenderingContext2D);
  const $input =
    inputCanvas && (inputCanvas.getContext("2d") as CanvasRenderingContext2D);
  const drawBg = function (
    x: number,
    y: number,
    r: number,
    g: number,
    b: number
  ) {
    if (!$bg) return;
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

  const run = function () {
    for (let x = 0; x <= 35; x++) {
      for (let y = 0; y <= 35; y++) {
        drawBg(x, y, R(x, y, t), G(x, y, t), B(x, y, t));
      }
    }
    t = t + 0.05;

    if (bgCanvas) {
      if ($input) {
        $input.drawImage(bgCanvas, 0, 0);
      }

      window.requestAnimationFrame(run);
    } else {
      console.error("no bgCanvas");
    }
  };

  run();
};
