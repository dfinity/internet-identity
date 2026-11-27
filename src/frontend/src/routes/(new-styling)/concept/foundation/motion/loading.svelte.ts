/**
 * Concept only: the identity's data arriving late, as it will from the
 * canister. Each `Load` stands for one request the product will make; until
 * it lands, the page shows a skeleton where its data goes (`Swap`), and
 * nothing that is worked out from it (a dot, the island, the dock).
 *
 * Two requests, as the product will have them: the account (how the identity
 * signs in, recovers, where it's signed in), which the panel's cards tell,
 * and its apps, which the grid shows. They land on their own.
 */

export type LoadState = "loading" | "ready" | "failed";

export class Load {
  state = $state<LoadState>("ready");
  #timer: ReturnType<typeof setTimeout> | undefined;

  /** Starts over: the data lands `after` ms from now, or fails then. At 0 it
   *  is there at once, in this same frame, so nothing waits and nothing
   *  fades. `onReady` runs as it lands. */
  start(after: number, { fails = false, onReady = () => {} } = {}): void {
    clearTimeout(this.#timer);
    if (after <= 0 && !fails) {
      this.state = "ready";
      onReady();
      return;
    }
    this.state = "loading";
    this.#timer = setTimeout(() => {
      this.state = fails ? "failed" : "ready";
      if (!fails) {
        onReady();
      }
    }, after);
  }
}

export type LoadSpeed = "instant" | "normal" | "slow" | "fails";

/** How long each request takes, picked in the concept menu: about what a
 *  query to the canister takes, and a slow network. */
export const LOAD_SPEEDS: {
  value: LoadSpeed;
  label: string;
  account: number;
  apps: number;
  appsFail?: boolean;
}[] = [
  { value: "instant", label: "At once", account: 0, apps: 0 },
  { value: "normal", label: "In under a second", account: 600, apps: 800 },
  { value: "slow", label: "Slowly, in 2.5 s", account: 2000, apps: 2500 },
  {
    value: "fails",
    label: "Apps fail to load",
    account: 600,
    apps: 1500,
    appsFail: true,
  },
];

/** Where the pick is kept, so it holds from visit to visit. */
export const LOAD_SPEED_KEY = "ii-concept-load-v1";

/** The pick, from the address (`?load=slow`) or as it was kept. */
export const loadSpeedOf = (asked: string | null): LoadSpeed =>
  LOAD_SPEEDS.find(({ value }) => value === asked)?.value ?? "normal";

export const saveLoadSpeed = (speed: LoadSpeed): void => {
  localStorage.setItem(LOAD_SPEED_KEY, speed);
};
