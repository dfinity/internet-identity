import { beforeEach, describe, expect, it } from "vitest";
import {
  readPromptParams,
  resolvePromptParams,
  stripPromptParams,
} from "./promptParams";

const PRINCIPAL = "2vxsx-fae";

describe("authorize prompt params", () => {
  beforeEach(() => {
    sessionStorage.clear();
    window.history.replaceState(null, "", "http://localhost:3000/authorize");
  });

  it("reads a silent request", () => {
    expect(
      readPromptParams(
        new URL(
          `http://localhost:3000/authorize?prompt=none&hint=${PRINCIPAL}`,
        ),
      ),
    ).toEqual({ prompt: "none", hint: PRINCIPAL });
  });

  it("reads an interactive request", () => {
    expect(
      readPromptParams(new URL("http://localhost:3000/authorize?prompt=login")),
    ).toEqual({ prompt: "login", hint: undefined });
  });

  it("treats an unknown prompt as absent", () => {
    expect(
      readPromptParams(
        new URL("http://localhost:3000/authorize?prompt=consent"),
      ),
    ).toEqual({ prompt: undefined, hint: undefined });
  });

  it("treats a hint that is not a principal as absent", () => {
    expect(
      readPromptParams(
        new URL("http://localhost:3000/authorize?hint=not-a-principal"),
      ),
    ).toEqual({ prompt: undefined, hint: undefined });
  });

  it("keeps the params across a resume", () => {
    resolvePromptParams(
      new URL(`http://localhost:3000/authorize?prompt=none&hint=${PRINCIPAL}`),
      false,
    );

    expect(
      resolvePromptParams(new URL("http://localhost:3000/authorize"), true),
    ).toEqual({ prompt: "none", hint: PRINCIPAL });
  });

  it("clears a stored context when a later request carries none", () => {
    resolvePromptParams(
      new URL("http://localhost:3000/authorize?prompt=none"),
      false,
    );

    resolvePromptParams(new URL("http://localhost:3000/authorize"), false);

    expect(
      resolvePromptParams(new URL("http://localhost:3000/authorize"), true),
    ).toEqual({});
  });

  it("strips the params it has consumed and leaves the rest", () => {
    window.history.replaceState(
      null,
      "",
      `http://localhost:3000/authorize?prompt=none&hint=${PRINCIPAL}&sso=example.com`,
    );

    stripPromptParams();

    const url = new URL(window.location.href);
    expect(url.searchParams.get("prompt")).toBeNull();
    expect(url.searchParams.get("hint")).toBeNull();
    expect(url.searchParams.get("sso")).toBe("example.com");
  });
});
