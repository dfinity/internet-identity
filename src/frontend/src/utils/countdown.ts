import { env } from "process";

export class Countdown {
  private timeoutHandle;
  private expected;
  private endTimestamp;

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
    console.log("step drift: " + drift);
    this.updateFunc();
    this.expected += this.interval;
    this.timeoutHandle = window.setTimeout(
      () => this.step(),
      Math.max(0, this.interval - drift)
    );
  }
}
