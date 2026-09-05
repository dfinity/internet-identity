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
    ).toEqual({ prompt: "none", hint: PRINCIPAL, resumable: undefined });
  });

  it("reads a request to be kept for later", () => {
    expect(
      readPromptParams(
        new URL("http://localhost:3000/authorize?resumable=true"),
      ),
    ).toEqual({ prompt: undefined, hint: undefined, resumable: true });
  });

  it("keeps nothing for an app that did not ask in so many words", () => {
    for (const value of ["false", "1", "yes", ""]) {
      expect(
        readPromptParams(
          new URL(`http://localhost:3000/authorize?resumable=${value}`),
        ).resumable,
      ).toBeUndefined();
    }
  });

  it("reads an interactive request", () => {
    expect(
      readPromptParams(new URL("http://localhost:3000/authorize?prompt=login")),
    ).toEqual({ prompt: "login", hint: undefined, resumable: undefined });
  });

  it("treats an unknown prompt as absent", () => {
    expect(
      readPromptParams(
        new URL("http://localhost:3000/authorize?prompt=consent"),
      ),
    ).toEqual({ prompt: undefined, hint: undefined, resumable: undefined });
  });

  it("treats a hint that is not a principal as absent", () => {
    expect(
      readPromptParams(
        new URL("http://localhost:3000/authorize?hint=not-a-principal"),
      ),
    ).toEqual({ prompt: undefined, hint: undefined, resumable: undefined });
  });

  it("keeps the params across a resume", () => {
    resolvePromptParams(
      new URL(
        `http://localhost:3000/authorize?prompt=none&hint=${PRINCIPAL}&resumable=true`,
      ),
      false,
    );

    expect(
      resolvePromptParams(new URL("http://localhost:3000/authorize"), true),
    ).toEqual({ prompt: "none", hint: PRINCIPAL, resumable: true });
  });

  it("keeps a lone request to be kept across a resume", () => {
    resolvePromptParams(
      new URL("http://localhost:3000/authorize?resumable=true"),
      false,
    );

    expect(
      resolvePromptParams(new URL("http://localhost:3000/authorize"), true)
        .resumable,
    ).toBe(true);
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
      `http://localhost:3000/authorize?prompt=none&hint=${PRINCIPAL}&resumable=true&sso=example.com`,
    );

    stripPromptParams();

    const url = new URL(window.location.href);
    expect(url.searchParams.get("prompt")).toBeNull();
    expect(url.searchParams.get("hint")).toBeNull();
    expect(url.searchParams.get("resumable")).toBeNull();
    expect(url.searchParams.get("sso")).toBe("example.com");
  });
});
