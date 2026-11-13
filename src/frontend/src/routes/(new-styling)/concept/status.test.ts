import { describe, expect, it } from "vitest";
import {
  deriveStatus,
  incidentOf,
  needsAttention,
  type Scenario,
} from "./status";

const calm: Scenario = {
  recovery: "calm",
  access: "calm",
  devices: "calm",
  integrations: "calm",
};

const derive = (
  scenario: Partial<Scenario>,
  signedOut: string[] = [],
  browserCount = 2,
) =>
  Object.fromEntries(
    deriveStatus({
      scenario: { ...calm, ...scenario },
      signedOut: new Set(signedOut),
      browserCount,
      apps: { total: 10, notifying: 4 },
      signedInWith: "iCloud Keychain",
    }).map(({ kind, tone, sentence }) => [kind, { tone, sentence }]),
  );

describe("deriveStatus", () => {
  it("keeps every card calm when nothing needs the user", () => {
    const cards = derive({});
    expect(Object.values(cards).map(({ tone }) => tone)).toEqual([
      "calm",
      "calm",
      "calm",
      "calm",
      "calm",
    ]);
    expect(cards.access.sentence).toBe("Signed in with iCloud Keychain");
    expect(cards.apps.sentence).toBe("4 of 10 apps can notify you");
  });

  it("tells a missing recovery once, by the access card", () => {
    const cards = derive({ recovery: "none", access: "one" });
    expect(cards.access.sentence).toBe("Only one way in, no recovery");
    // Recovery leads, so the access card's to-do waits behind it.
    expect(cards.recovery.tone).toBe("action");
    expect(cards.access.tone).toBe("queued");
  });

  it("lets only one card ask for action", () => {
    const cards = derive({ recovery: "unverified", access: "one" });
    const loud = Object.values(cards).filter(({ tone }) => tone === "action");
    expect(loud).toHaveLength(1);
  });

  it("asks for a passkey while signed in with the recovery phrase", () => {
    const cards = derive({
      recovery: "usedPhrase",
      integrations: "cliConfirm",
    });
    expect(cards.recovery.tone).toBe("now");
    expect(cards.access).toEqual({
      tone: "action",
      sentence: "Add a passkey to sign in next time",
    });
    // Out of focus while the situation lasts.
    expect(cards.integrations.tone).toBe("tip");
  });

  it("flags a phrase reset from a new browser as a risk", () => {
    const cards = derive({
      recovery: "phraseReset",
      access: "newPasskey",
      devices: "newBrowser",
    });
    expect(cards.recovery.tone).toBe("risk");
    expect(cards.devices.tone).toBe("risk");
    expect(cards.access.sentence).toBe("New passkey from Firefox");
  });

  it("calms down once the new browser is signed out", () => {
    const cards = derive(
      { recovery: "phraseReset", access: "newPasskey", devices: "newBrowser" },
      ["firefox"],
    );
    expect(cards.devices).toEqual({
      tone: "recent",
      sentence: "Firefox was signed out",
    });
    expect(cards.recovery.tone).toBe("recent");
  });

  it("drops other to-dos to tips while there is a risk", () => {
    const cards = derive({ integrations: "local", recovery: "unverified" });
    expect(cards.integrations.tone).toBe("risk");
    expect(cards.recovery.tone).toBe("tip");
  });
});

describe("needsAttention", () => {
  it("is set by a to-do and not by news", () => {
    const of = (scenario: Partial<Scenario>) =>
      needsAttention(
        deriveStatus({
          scenario: { ...calm, ...scenario },
          signedOut: new Set(),
          browserCount: 2,
          apps: { total: 1, notifying: 0 },
          signedInWith: "a passkey",
        }),
      );
    expect(of({ devices: "newBrowser" })).toBe(false);
    expect(of({ recovery: "unverified" })).toBe(true);
    expect(of({ integrations: "local" })).toBe(true);
  });
});

describe("incidentOf", () => {
  const incident = (scenario: Partial<Scenario>, signedOut: string[] = []) =>
    incidentOf({
      scenario: { ...calm, ...scenario },
      signedOut: new Set(signedOut),
    });
  const firefox: Partial<Scenario> = {
    recovery: "phraseReset",
    access: "newPasskey",
    devices: "newBrowser",
  };

  it("tells a phrase reset from a new browser as one story", () => {
    expect(incident(firefox)?.kinds).toEqual(["devices", "access", "recovery"]);
    expect(incident(firefox)?.facts.map(({ kind }) => kind)).toEqual([
      "devices",
      "access",
      "recovery",
    ]);
  });

  it("ends once the new browser is signed out", () => {
    expect(incident(firefox, ["firefox"])).toBeUndefined();
  });

  it("goes quiet whichever way it is answered", () => {
    const answered = incident(firefox);
    for (const answer of [answered?.secure, answered?.confirm]) {
      const after = { ...calm, ...firefox, ...answer?.patch };
      expect(incident(after)).toBeUndefined();
      expect(derive(after).recovery.tone).not.toBe("risk");
      expect(derive(after).devices.tone).not.toBe("risk");
    }
  });

  it("asks about a local server left open, and keeps it quietly", () => {
    const local = incident({ integrations: "local" });
    expect(local?.kinds).toEqual(["integrations"]);
    expect(derive({ ...local?.confirm.patch }).integrations.tone).toBe("tip");
  });

  it("is not raised by news or a to-do", () => {
    expect(incident({ devices: "newBrowser" })).toBeUndefined();
    expect(incident({ recovery: "unverified" })).toBeUndefined();
  });
});
