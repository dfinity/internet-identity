import { beforeEach, describe, expect, it } from "vitest";
import { resolvePromptParams, stripPromptParams } from "./promptParams";

// Any valid textual principal; the parser only cares that it decodes.
const PRINCIPAL = "2vxsx-fae";
const NOT_RESUMING = false;
const RESUMING = true;

// Built against the test environment's own origin: `replaceState` refuses to
// cross origins, and the code under test rewrites the real address bar.
const authorizeUrl = (query: string): URL =>
  new URL(`/authorize${query}`, window.location.origin);

const visit = (query: string): void => {
  window.history.replaceState(null, "", authorizeUrl(query));
};

beforeEach(() => {
  sessionStorage.clear();
  visit("");
});

describe("resolvePromptParams", () => {
  it("reads both params", () => {
    expect(
      resolvePromptParams(
        authorizeUrl(`?prompt=none&hint=${PRINCIPAL}`),
        NOT_RESUMING,
      ),
    ).toEqual({ prompt: "none", hint: PRINCIPAL });
  });

  it("reads nothing from a plain authorize URL", () => {
    expect(resolvePromptParams(authorizeUrl(""), NOT_RESUMING)).toEqual({
      prompt: undefined,
      hint: undefined,
    });
  });

  it.each(["consent", "select_account", "", "NONE", "banana"])(
    "ignores the unimplemented prompt value %j",
    (value) => {
      // Falling through to default behaviour rather than failing means an app
      // written against the wider OpenID Connect vocabulary still signs in.
      expect(
        resolvePromptParams(authorizeUrl(`?prompt=${value}`), NOT_RESUMING)
          .prompt,
      ).toBeUndefined();
    },
  );

  it("drops a hint that is not a principal", () => {
    expect(
      resolvePromptParams(authorizeUrl("?hint=not-a-principal"), NOT_RESUMING)
        .hint,
    ).toBeUndefined();
  });

  it("keeps the params across an identity provider round-trip", () => {
    resolvePromptParams(
      authorizeUrl(`?prompt=login&hint=${PRINCIPAL}`),
      NOT_RESUMING,
    );

    // The resumed load arrives with the params gone from the URL. Without this,
    // a `prompt=login` request would be read as asking for nothing and answered
    // from the very delegation the user was just made to sign in past.
    expect(
      resolvePromptParams(authorizeUrl("?flow=openid-resume"), RESUMING),
    ).toEqual({ prompt: "login", hint: PRINCIPAL });
  });

  it("clears a stored value on any load that is not resuming", () => {
    resolvePromptParams(authorizeUrl("?prompt=login"), NOT_RESUMING);
    resolvePromptParams(authorizeUrl(""), NOT_RESUMING);

    // A value must not outlive the flow that set it and be picked up by an
    // unrelated later one in the same tab.
    expect(
      resolvePromptParams(authorizeUrl("?flow=openid-resume"), RESUMING).prompt,
    ).toBeUndefined();
  });

  it("prefers the URL over a stored value when resuming with both", () => {
    resolvePromptParams(authorizeUrl("?prompt=login"), NOT_RESUMING);

    expect(
      resolvePromptParams(authorizeUrl("?prompt=none"), RESUMING).prompt,
    ).toBe("login");
  });
});

describe("stripPromptParams", () => {
  it("removes both params and keeps the rest of the URL", () => {
    visit(`?openid=x&prompt=none&hint=${PRINCIPAL}#frag`);

    stripPromptParams();

    // A principal in the address bar would linger in history, and a copied URL
    // could replay a silent sign-in.
    expect(window.location.search).toBe("?openid=x");
    expect(window.location.hash).toBe("#frag");
  });

  it("leaves a URL without the params alone", () => {
    visit("?openid=x");

    stripPromptParams();

    expect(window.location.search).toBe("?openid=x");
  });
});
