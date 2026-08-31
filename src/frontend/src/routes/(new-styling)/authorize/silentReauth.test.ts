import { describe, expect, it } from "vitest";
import { chooseSilentSession } from "./silentReauth";

const PRINCIPAL_A = "2vxsx-fae";
const PRINCIPAL_B = "aaaaa-aa";

const held = (accountPrincipal: string) => ({ accountPrincipal });

describe("chooseSilentSession", () => {
  it("denies when this browser holds nothing for the origin", () => {
    expect(chooseSilentSession({ held: [] })).toEqual({
      denial: "login_required",
    });
  });

  it("uses the only session held when no hint is given", () => {
    expect(chooseSilentSession({ held: [held(PRINCIPAL_A)] })).toEqual({
      session: held(PRINCIPAL_A),
    });
  });

  it("asks rather than guessing between personas", () => {
    expect(
      chooseSilentSession({
        held: [held(PRINCIPAL_A), held(PRINCIPAL_B)],
      }),
    ).toEqual({ denial: "account_selection_required" });
  });

  it("selects the hinted session", () => {
    expect(
      chooseSilentSession({
        held: [held(PRINCIPAL_A), held(PRINCIPAL_B)],
        hint: PRINCIPAL_B,
      }),
    ).toEqual({ session: held(PRINCIPAL_B) });
  });

  it("denies a hint this browser holds no session for", () => {
    expect(
      chooseSilentSession({
        held: [held(PRINCIPAL_A)],
        hint: PRINCIPAL_B,
      }),
    ).toEqual({ denial: "login_required" });
  });

  it("denies rather than guessing when a hint is ambiguous", () => {
    expect(
      chooseSilentSession({
        held: [held(PRINCIPAL_A), held(PRINCIPAL_A)],
        hint: PRINCIPAL_A,
      }),
    ).toEqual({ denial: "account_selection_required" });
  });
});
