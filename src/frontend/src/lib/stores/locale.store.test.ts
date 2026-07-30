import { get } from "svelte/store";
import { formatDuration } from "./locale.store";

// The locale isn't switched here: loading a catalog needs the lingui Vite
// plugin, which the test setup doesn't run. These cover the formatting itself,
// in the default locale.
describe("formatDuration", () => {
  it("spells out and pluralizes a single unit", () => {
    expect(get(formatDuration)({ minute: 20 })).toBe("20 minutes");
    expect(get(formatDuration)({ hour: 1 })).toBe("1 hour");
    expect(get(formatDuration)({ day: 5 })).toBe("5 days");
  });

  it("joins several units", () => {
    expect(get(formatDuration)({ day: 1, hour: 1, minute: 1 })).toBe(
      "1 day 1 hour 1 minute",
    );
  });

  it("only formats the units it's given", () => {
    expect(get(formatDuration)({ second: 0 })).toBe("0 seconds");
    expect(get(formatDuration)({})).toBe("");
  });
});
