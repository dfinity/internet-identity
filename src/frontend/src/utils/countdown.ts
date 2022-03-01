import {render} from "lit-html";
import {displayError} from "../components/displayError";

export class Countdown {
  private timeoutHandle;
  private expected;
  private readonly endTimestamp;

  constructor(
    private updateFunc: () => void,
    private interval: number,
    private timeoutFunc: () => void,
    endTimestamp: bigint
  ) {
    this.expected = Date.now() + this.interval;
    this.timeoutHandle = window.setTimeout(() => this.step(), this.interval);
    this.endTimestamp = Number(endTimestamp / BigInt("1000000"));
  }

  public stop(): void {
    window.clearTimeout(this.timeoutHandle);
  }

  private async step() {
    const now = Date.now();
    if (now >= this.endTimestamp) {
      await this.timeoutFunc();
      return;
    }
    const drift = now - this.expected;
    this.updateFunc();
    this.expected += this.interval;
    this.timeoutHandle = window.setTimeout(
      () => this.step(),
      Math.max(0, this.interval - drift)
    );
  }
}

export const setupCountdown = (
  timestamp: bigint,
  continueFunc: () => Promise<void>
): Countdown => {
  const timer = document.getElementById("timer") as HTMLElement;
  return new Countdown(
    () => render(formatRemainingTime(timestamp), timer),
    1000,
    async () => {
      await displayError({
        title: "Timeout reached",
        message:
          "The timeout has been reached. For security reasons, the add device process has been aborted.",
        primaryButton: "Ok",
      });
      await continueFunc;
    },
    timestamp
  );
};

export function formatRemainingTime(endTimestamp: bigint): string {
  const [minRemaining, secondsRemaining] = calculateTimeRemaining(endTimestamp)
  return `${minRemaining}:${secondsRemaining}`;
}

const calculateTimeRemaining = (
  expirationTimestamp: bigint
): [string, string] => {
  const now = new Date().getTime();
  const diffSeconds =
    (Number(expirationTimestamp / BigInt("1000000")) - now) / 1000;
  return [
    Math.floor(diffSeconds / 60).toString(),
    Math.floor(diffSeconds % 60)
      .toString()
      .padStart(2, "0"),
  ];
};
