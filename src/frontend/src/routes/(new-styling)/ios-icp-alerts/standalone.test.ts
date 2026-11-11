import { afterEach, describe, expect, it } from "vitest";
import { isStandalone } from "./standalone";

/** Defined rather than spied on: jsdom serves no `matchMedia`. */
const displayMode = (matching?: string): void => {
  Object.defineProperty(window, "matchMedia", {
    configurable: true,
    value: (query: string) => ({
      matches: query === `(display-mode: ${matching})`,
    }),
  });
};

/** What iOS answered with before it supported `display-mode`, which it still answers. */
const legacyStandalone = (value: boolean | undefined): void => {
  Object.defineProperty(navigator, "standalone", {
    configurable: true,
    value,
  });
};

afterEach(() => {
  legacyStandalone(undefined);
});

describe("isStandalone", () => {
  it("says a browser tab is not the installed app", () => {
    displayMode();

    expect(isStandalone()).toBe(false);
  });

  it.each(["standalone", "fullscreen"])(
    "reads the installed app off display-mode %s",
    (mode) => {
      displayMode(mode);

      expect(isStandalone()).toBe(true);
    },
  );

  it("reads the installed app off navigator.standalone", () => {
    displayMode();
    legacyStandalone(true);

    expect(isStandalone()).toBe(true);
  });

  it("does not read a browser tab off navigator.standalone", () => {
    displayMode();
    legacyStandalone(false);

    expect(isStandalone()).toBe(false);
  });
});
