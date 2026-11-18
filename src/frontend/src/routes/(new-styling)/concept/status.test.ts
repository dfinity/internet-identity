import { describe, expect, it } from "vitest";
import {
  aiAccessOn,
  cliAccessOn,
  deriveStatus,
  incidentOf,
  needsAttention,
  withAiAccess,
  withCliAccess,
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
    }).map(({ kind, tone, sentence }) => [kind, { tone, sentence }]),
  );

describe("deriveStatus", () => {
  it("keeps every card neutral when nothing needs the user", () => {
    const cards = derive({});
    expect(Object.values(cards).map(({ tone }) => tone)).toEqual([
      "neutral",
      "neutral",
      "neutral",
      "neutral",
      "neutral",
    ]);
    expect(cards.access.sentence).toBe("3 ways to sign in");
    expect(cards.recovery.sentence).toBe("Phrase and email set up");
    expect(cards.devices.sentence).toBe("Signed in on 2 browsers");
    expect(cards.apps.sentence).toBe("4 of 10 apps can notify you");
  });

  it("takes one way back in as enough", () => {
    expect(derive({ recovery: "phraseOnly" }).recovery).toEqual({
      tone: "neutral",
      sentence: "Recovery phrase set up",
    });
    expect(derive({ recovery: "emailOnly" }).recovery).toEqual({
      tone: "neutral",
      sentence: "Recovery email set up",
    });
  });

  it("suggests recovery, and urges it for a passkey on this device alone", () => {
    const synced = derive({ recovery: "none", access: "one" });
    expect(synced.recovery).toEqual({
      tone: "info",
      sentence: "Not set up yet",
    });
    expect(synced.access).toEqual({
      tone: "neutral",
      sentence: "1 way to sign in",
    });
    expect(derive({ recovery: "none", access: "oneLocal" }).recovery).toEqual({
      tone: "warning",
      sentence: "Only this device can sign you in",
    });
  });

  it("lets only one card urge a step, whatever the state", () => {
    const recovery = [
      "none",
      "usedPhrase",
      "phraseReset",
      "phraseOnly",
      "emailOnly",
      "calm",
    ] as const;
    const access = ["one", "oneLocal", "unused", "calm"] as const;
    const devices = ["newBrowser", "known", "many", "calm"] as const;
    for (const r of recovery) {
      for (const a of access) {
        for (const d of devices) {
          const cards = Object.values(
            derive({ recovery: r, access: a, devices: d }),
          );
          expect(
            cards.filter(({ tone }) => tone === "warning").length,
          ).toBeLessThanOrEqual(1);
        }
      }
    }
  });

  it("urges a new way in while signed in with the recovery phrase", () => {
    const cards = derive({
      recovery: "usedPhrase",
      integrations: "cliConfirm",
    });
    expect(cards.recovery).toEqual({
      tone: "info",
      sentence: "Signed in with your recovery phrase",
    });
    expect(cards.access).toEqual({
      tone: "warning",
      sentence: "Add a way to sign in",
    });
    expect(cards.integrations.tone).toBe("info");
  });

  it("flags a phrase change from a new browser as a risk on both cards", () => {
    const cards = derive({ recovery: "phraseReset", devices: "newBrowser" });
    expect(cards.recovery).toEqual({
      tone: "risk",
      sentence: "Recovery phrase changed from Firefox",
    });
    expect(cards.devices).toEqual({
      tone: "risk",
      sentence: "New sign-in from Firefox",
    });
    expect(cards.access.tone).toBe("neutral");
  });

  it("tells a new sign-in alone as info", () => {
    expect(derive({ devices: "newBrowser" }).devices).toEqual({
      tone: "info",
      sentence: "New sign-in from Firefox",
    });
  });

  it("calms down once the new browser is signed out", () => {
    const cards = derive({ recovery: "phraseReset", devices: "newBrowser" }, [
      "firefox",
    ]);
    expect(cards.devices).toEqual({
      tone: "neutral",
      sentence: "Signed in on 2 browsers",
    });
    expect(cards.recovery.tone).toBe("info");
  });

  it("drops other warnings to info while there is a risk", () => {
    const cards = derive({
      integrations: "local",
      recovery: "none",
      access: "oneLocal",
    });
    expect(cards.integrations.tone).toBe("risk");
    expect(cards.recovery.tone).toBe("info");
  });
});

describe("needsAttention", () => {
  it("is set by a step and not by news", () => {
    const of = (scenario: Partial<Scenario>) =>
      needsAttention(
        deriveStatus({
          scenario: { ...calm, ...scenario },
          signedOut: new Set(),
          browserCount: 2,
          apps: { total: 1, notifying: 0 },
        }),
      );
    expect(of({})).toBe(false);
    expect(of({ devices: "newBrowser" })).toBe(false);
    expect(of({ recovery: "none" })).toBe(true);
    expect(of({ recovery: "none", access: "oneLocal" })).toBe(true);
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
    devices: "newBrowser",
  };

  it("tells a phrase change from a new browser as one story", () => {
    expect(incident(firefox)?.kinds).toEqual(["devices", "recovery"]);
    expect(incident(firefox)?.facts.map(({ kind }) => kind)).toEqual([
      "devices",
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
    expect(derive({ ...local?.confirm.patch }).integrations.tone).toBe(
      "neutral",
    );
  });

  it("is not raised by news or a to-do", () => {
    expect(incident({ devices: "newBrowser" })).toBeUndefined();
    expect(incident({ recovery: "none" })).toBeUndefined();
  });
});

describe("switching AI and CLI access", () => {
  it("switches each on its own", () => {
    expect(withAiAccess("calm", false)).toBe("cliOnly");
    expect(withCliAccess("calm", false)).toBe("aiOnly");
    expect(withAiAccess("off", true)).toBe("aiOnly");
    expect(withCliAccess("aiOnly", true)).toBe("calm");
    expect(withCliAccess("cliOnly", false)).toBe("off");
  });

  it("forgets a local server along with AI access", () => {
    expect(withAiAccess("local", false)).toBe("cliOnly");
  });

  it("approves a waiting CLI on, and declines it off", () => {
    expect(cliAccessOn("cliConfirm")).toBe(false);
    expect(withCliAccess("cliConfirm", true)).toBe("calm");
    expect(withCliAccess("cliConfirm", false)).toBe("aiOnly");
  });

  it("has no state for AI off while the CLI waits, or CLI off beside a local server", () => {
    expect(withAiAccess("cliConfirm", false)).toBeUndefined();
    expect(withCliAccess("local", false)).toBeUndefined();
    expect(withCliAccess("localAllowed", false)).toBeUndefined();
  });

  it("lands on a state that says what it switched", () => {
    for (const state of ["calm", "aiOnly", "cliOnly", "off"] as const) {
      for (const on of [true, false]) {
        const ai = withAiAccess(state, on);
        const cli = withCliAccess(state, on);
        expect(ai !== undefined && aiAccessOn(ai)).toBe(on);
        expect(cli !== undefined && cliAccessOn(cli)).toBe(on);
      }
    }
  });
});
