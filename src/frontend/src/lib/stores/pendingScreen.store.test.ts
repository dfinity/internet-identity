import { describe, expect, it } from "vitest";
import { get } from "svelte/store";
import { claimScreen, pendingScreenStore } from "./pendingScreen.store";

describe("pendingScreenStore", () => {
  it("owes nothing until something claims the screen", () => {
    expect(get(pendingScreenStore)).toBe(false);
  });

  it("is owed from the claim until the release", () => {
    const release = claimScreen();
    expect(get(pendingScreenStore)).toBe(true);
    release();
    expect(get(pendingScreenStore)).toBe(false);
  });

  /** An app may have several requests in flight, and the screen stays claimed until
   *  the last of them has answered for itself. */
  it("stays owed while another claim is outstanding", () => {
    const first = claimScreen();
    const second = claimScreen();
    first();
    expect(get(pendingScreenStore)).toBe(true);
    second();
    expect(get(pendingScreenStore)).toBe(false);
  });

  /** Released from a `finally`, which can run twice for one claim where a handler
   *  nests them. Counting a release twice would free a claim that is still held. */
  it("ignores a release it has already seen", () => {
    const release = claimScreen();
    const other = claimScreen();
    release();
    release();
    expect(get(pendingScreenStore)).toBe(true);
    other();
    expect(get(pendingScreenStore)).toBe(false);
  });
});
