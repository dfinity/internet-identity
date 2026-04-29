import { resolveAttributeGroups } from "./attributes";

const GOOGLE_ISSUER = "https://accounts.google.com";
const APPLE_ISSUER = "https://appleid.apple.com";
const MICROSOFT_ISSUER = "https://login.microsoftonline.com/{tid}/v2.0";
const SSO_DOMAIN = "dfinity.org";

const encoder = new TextEncoder();
const bytes = (s: string): Uint8Array => encoder.encode(s);

type Available = Array<[string, Uint8Array | number[]]>;

// vitest 4's `toEqual` on `Uint8Array` instances backed by different
// `ArrayBuffer`s reports "no visual difference" yet fails (the assertion
// compares buffer identity). Compare element-wise via plain arrays so the
// content equality is what we're actually asserting on.
const arrayOf = (u: Uint8Array): number[] => Array.from(u);

// Mirrors what `list_available_attributes` would return on a fully-linked
// anchor: every supported issuer/domain populated with `name`, `email`, and
// (where the canister certifies it) `verified_email`. SSO intentionally has
// no `verified_email` — see PR #3805 / `isOneClickSsoKey` in attributes.ts.
const fullyAvailable: Available = [
  [`openid:${GOOGLE_ISSUER}:name`, bytes("Google Name")],
  [`openid:${GOOGLE_ISSUER}:email`, bytes("user@google.com")],
  [`openid:${GOOGLE_ISSUER}:verified_email`, bytes("user@google.com")],
  [`openid:${APPLE_ISSUER}:name`, bytes("Apple Name")],
  [`openid:${APPLE_ISSUER}:email`, bytes("user@apple.com")],
  [`openid:${APPLE_ISSUER}:verified_email`, bytes("user@apple.com")],
  [`openid:${MICROSOFT_ISSUER}:name`, bytes("Microsoft Name")],
  [`openid:${MICROSOFT_ISSUER}:email`, bytes("user@microsoft.com")],
  [`openid:${MICROSOFT_ISSUER}:verified_email`, bytes("user@microsoft.com")],
  [`sso:${SSO_DOMAIN}:name`, bytes("SSO Name")],
  [`sso:${SSO_DOMAIN}:email`, bytes("user@dfinity.org")],
];

describe("resolveAttributeGroups", () => {
  describe("input edges", () => {
    it("returns no groups when no keys are requested", () => {
      expect(resolveAttributeGroups([], fullyAvailable)).toEqual([]);
    });

    it("returns no groups when nothing is available", () => {
      expect(
        resolveAttributeGroups(
          ["name", "email", `openid:${GOOGLE_ISSUER}:name`],
          [],
        ),
      ).toEqual([]);
    });

    it("omits requested keys that don't match any available attribute", () => {
      expect(
        resolveAttributeGroups(
          ["favorite_color", `openid:${GOOGLE_ISSUER}:favorite_color`],
          fullyAvailable,
        ),
      ).toEqual([]);
    });

    it("does not conflate `email` with `verified_email` via suffix matching", () => {
      // `email` only suffix-matches keys ending in `:email`, so the Google
      // `verified_email` entry must not bleed into the `email` group.
      const groups = resolveAttributeGroups(
        ["email"],
        [
          [`openid:${GOOGLE_ISSUER}:verified_email`, bytes("v@g.com")],
          [`openid:${GOOGLE_ISSUER}:email`, bytes("e@g.com")],
        ],
      );
      expect(groups).toHaveLength(1);
      expect(groups[0].name).toBe("email");
      expect(groups[0].options).toHaveLength(1);
      expect(groups[0].options[0].key).toBe(`openid:${GOOGLE_ISSUER}:email`);
    });
  });

  describe("unscoped requests", () => {
    it("returns one option per available scope and marks `omitScope` true", () => {
      const groups = resolveAttributeGroups(["name"], fullyAvailable);
      expect(groups).toHaveLength(1);
      expect(groups[0].name).toBe("name");
      expect(
        groups[0].options.map((o) => ({
          key: o.key,
          displayValue: o.displayValue,
          rawValue: arrayOf(o.rawValue),
          omitScope: o.omitScope,
        })),
      ).toEqual([
        {
          key: `openid:${GOOGLE_ISSUER}:name`,
          displayValue: "Google Name",
          rawValue: arrayOf(bytes("Google Name")),
          omitScope: true,
        },
        {
          key: `openid:${APPLE_ISSUER}:name`,
          displayValue: "Apple Name",
          rawValue: arrayOf(bytes("Apple Name")),
          omitScope: true,
        },
        {
          key: `openid:${MICROSOFT_ISSUER}:name`,
          displayValue: "Microsoft Name",
          rawValue: arrayOf(bytes("Microsoft Name")),
          omitScope: true,
        },
        {
          key: `sso:${SSO_DOMAIN}:name`,
          displayValue: "SSO Name",
          rawValue: arrayOf(bytes("SSO Name")),
          omitScope: true,
        },
      ]);
    });

    it("collects `email` from every issuer/domain that has it", () => {
      const groups = resolveAttributeGroups(["email"], fullyAvailable);
      expect(groups).toHaveLength(1);
      expect(groups[0].name).toBe("email");
      expect(groups[0].options.map((o) => o.key)).toEqual([
        `openid:${GOOGLE_ISSUER}:email`,
        `openid:${APPLE_ISSUER}:email`,
        `openid:${MICROSOFT_ISSUER}:email`,
        `sso:${SSO_DOMAIN}:email`,
      ]);
    });

    it("collects `verified_email` only from openid scopes", () => {
      const groups = resolveAttributeGroups(["verified_email"], fullyAvailable);
      expect(groups).toHaveLength(1);
      expect(groups[0].name).toBe("verified_email");
      expect(groups[0].options.map((o) => o.key)).toEqual([
        `openid:${GOOGLE_ISSUER}:verified_email`,
        `openid:${APPLE_ISSUER}:verified_email`,
        `openid:${MICROSOFT_ISSUER}:verified_email`,
      ]);
    });

    it("yields a single-option group when only one scope is available", () => {
      const groups = resolveAttributeGroups(
        ["name"],
        [[`openid:${GOOGLE_ISSUER}:name`, bytes("Google Name")]],
      );
      expect(groups).toHaveLength(1);
      expect(groups[0].options).toHaveLength(1);
      expect(groups[0].options[0].omitScope).toBe(true);
    });
  });

  describe("scoped requests", () => {
    it.each([
      [`openid:${GOOGLE_ISSUER}:name`, "Google Name"],
      [`openid:${GOOGLE_ISSUER}:email`, "user@google.com"],
      [`openid:${GOOGLE_ISSUER}:verified_email`, "user@google.com"],
      [`openid:${APPLE_ISSUER}:name`, "Apple Name"],
      [`openid:${APPLE_ISSUER}:email`, "user@apple.com"],
      [`openid:${APPLE_ISSUER}:verified_email`, "user@apple.com"],
      [`openid:${MICROSOFT_ISSUER}:name`, "Microsoft Name"],
      [`openid:${MICROSOFT_ISSUER}:email`, "user@microsoft.com"],
      [`openid:${MICROSOFT_ISSUER}:verified_email`, "user@microsoft.com"],
      [`sso:${SSO_DOMAIN}:name`, "SSO Name"],
      [`sso:${SSO_DOMAIN}:email`, "user@dfinity.org"],
    ])(
      "exact-matches %s and marks `omitScope` false",
      (requestedKey, displayValue) => {
        const groups = resolveAttributeGroups([requestedKey], fullyAvailable);
        expect(groups).toHaveLength(1);
        expect(groups[0].name).toBe(
          requestedKey.slice(requestedKey.lastIndexOf(":") + 1),
        );
        expect(groups[0].options).toHaveLength(1);
        const [option] = groups[0].options;
        expect(option.key).toBe(requestedKey);
        expect(option.displayValue).toBe(displayValue);
        expect(arrayOf(option.rawValue)).toEqual(arrayOf(bytes(displayValue)));
        expect(option.omitScope).toBe(false);
      },
    );

    it("omits the group when the requested scope isn't in the anchor", () => {
      // Anchor only has Google-scoped keys, request a different issuer.
      const available: Available = [
        [`openid:${GOOGLE_ISSUER}:email`, bytes("user@google.com")],
      ];
      expect(
        resolveAttributeGroups([`openid:${APPLE_ISSUER}:email`], available),
      ).toEqual([]);
    });
  });

  describe("combinations", () => {
    it("returns one group per matched key, in request order", () => {
      const groups = resolveAttributeGroups(
        [
          "name",
          "email",
          `openid:${GOOGLE_ISSUER}:verified_email`,
          `sso:${SSO_DOMAIN}:name`,
        ],
        fullyAvailable,
      );
      expect(groups.map((g) => g.name)).toEqual([
        "name",
        "email",
        "verified_email",
        "name",
      ]);
      expect(
        groups.map((g) => ({
          options: g.options.length,
          omitScope: g.options.every((o) => o.omitScope),
        })),
      ).toEqual([
        { options: 4, omitScope: true },
        { options: 4, omitScope: true },
        { options: 1, omitScope: false },
        { options: 1, omitScope: false },
      ]);
    });

    it("requesting an unscoped key alongside its scoped form yields two distinct groups", () => {
      // No deduplication — the consent UI is responsible for handling
      // multiple groups with the same name.
      const groups = resolveAttributeGroups(
        [`openid:${GOOGLE_ISSUER}:name`, "name"],
        fullyAvailable,
      );
      expect(groups).toHaveLength(2);
      expect(groups[0]).toMatchObject({
        name: "name",
        options: [{ key: `openid:${GOOGLE_ISSUER}:name`, omitScope: false }],
      });
      expect(groups[1].name).toBe("name");
      expect(groups[1].options.every((o) => o.omitScope)).toBe(true);
      expect(groups[1].options).toHaveLength(4);
    });

    it("handles every documented attribute in one request", () => {
      const requestedKeys = [
        "name",
        "email",
        "verified_email",
        `openid:${GOOGLE_ISSUER}:name`,
        `openid:${GOOGLE_ISSUER}:email`,
        `openid:${GOOGLE_ISSUER}:verified_email`,
        `openid:${APPLE_ISSUER}:name`,
        `openid:${APPLE_ISSUER}:email`,
        `openid:${APPLE_ISSUER}:verified_email`,
        `openid:${MICROSOFT_ISSUER}:name`,
        `openid:${MICROSOFT_ISSUER}:email`,
        `openid:${MICROSOFT_ISSUER}:verified_email`,
        `sso:${SSO_DOMAIN}:name`,
        `sso:${SSO_DOMAIN}:email`,
      ];
      const groups = resolveAttributeGroups(requestedKeys, fullyAvailable);
      // Every requested key resolves: 3 unscoped + 11 scoped = 14 groups.
      expect(groups).toHaveLength(requestedKeys.length);
      // Unscoped requests fan out across all scopes; scoped requests stay
      // single-option.
      expect(groups.slice(0, 3).map((g) => g.options.length)).toEqual([
        4, 4, 3,
      ]);
      expect(groups.slice(3).every((g) => g.options.length === 1)).toBe(true);
      expect(
        groups.slice(0, 3).every((g) => g.options.every((o) => o.omitScope)),
      ).toBe(true);
      expect(
        groups.slice(3).every((g) => g.options.every((o) => !o.omitScope)),
      ).toBe(true);
    });

    it("only emits available scopes when the anchor has a partial set", () => {
      // Anchor linked to Google + SSO only; ask for something every provider
      // could satisfy.
      const partial: Available = [
        [`openid:${GOOGLE_ISSUER}:name`, bytes("Google Name")],
        [`sso:${SSO_DOMAIN}:name`, bytes("SSO Name")],
      ];
      const groups = resolveAttributeGroups(["name"], partial);
      expect(groups).toHaveLength(1);
      expect(groups[0].options.map((o) => o.key)).toEqual([
        `openid:${GOOGLE_ISSUER}:name`,
        `sso:${SSO_DOMAIN}:name`,
      ]);
    });
  });

  describe("regression scenarios", () => {
    it("keeps unscoped fan-out and exact match distinguishable when both resolve to a single Google option", () => {
      // Consent UI showed two identical-looking rows (same value, no
      // scope label) when the anchor was linked to only one provider:
      // unscoped `name` fanned out to a single Google option AND the
      // scoped `openid:google:name` request also produced one option.
      // The function must keep them distinguishable via `omitScope` so
      // the UI can render the exact request as scoped.
      const googleOnly: Available = [
        [`openid:${GOOGLE_ISSUER}:name`, bytes("Arshavir Ter-Gabrielyan")],
        [`openid:${GOOGLE_ISSUER}:email`, bytes("a@dfinity.org")],
      ];
      const groups = resolveAttributeGroups(
        [
          "name",
          "email",
          `openid:${GOOGLE_ISSUER}:name`,
          `openid:${GOOGLE_ISSUER}:email`,
        ],
        googleOnly,
      );
      expect(groups).toHaveLength(4);
      expect(
        groups.map((g) => ({
          name: g.name,
          omitScope: g.options[0].omitScope,
          key: g.options[0].key,
        })),
      ).toEqual([
        { name: "name", omitScope: true, key: `openid:${GOOGLE_ISSUER}:name` },
        {
          name: "email",
          omitScope: true,
          key: `openid:${GOOGLE_ISSUER}:email`,
        },
        { name: "name", omitScope: false, key: `openid:${GOOGLE_ISSUER}:name` },
        {
          name: "email",
          omitScope: false,
          key: `openid:${GOOGLE_ISSUER}:email`,
        },
      ]);
    });

    it("expected-2-scoped-1-unscoped: an unscoped request alongside two scoped requests yields three groups, not three fan-outs", () => {
      // Consent UI showed all rows looking unscoped when the user
      // requested `name` plus two specific scoped names. The two scoped
      // requests must surface as their own single-option groups with
      // `omitScope: false`; the unscoped one fans out across all scopes
      // with `omitScope: true`.
      const groups = resolveAttributeGroups(
        ["name", `openid:${GOOGLE_ISSUER}:name`, `openid:${APPLE_ISSUER}:name`],
        fullyAvailable,
      );
      expect(groups).toHaveLength(3);
      // Unscoped fan-out — multiple options, all omitScope: true.
      expect(groups[0].options.length).toBeGreaterThan(1);
      expect(groups[0].options.every((o) => o.omitScope)).toBe(true);
      // Two scoped exact matches — one option each, omitScope: false.
      expect(groups[1].options).toEqual([
        expect.objectContaining({
          key: `openid:${GOOGLE_ISSUER}:name`,
          omitScope: false,
        }),
      ]);
      expect(groups[2].options).toEqual([
        expect.objectContaining({
          key: `openid:${APPLE_ISSUER}:name`,
          omitScope: false,
        }),
      ]);
    });

    it("every option carries the full scoped key so the UI can derive a scope label", () => {
      // Consent UI dropped scope labels on some rows. Independent of
      // `omitScope`, every option's `key` must be the full
      // `<scope>:<name>` form so `extractScope` returns a non-empty
      // value for label rendering.
      const requestedKeys = [
        "name",
        "email",
        "verified_email",
        `openid:${GOOGLE_ISSUER}:name`,
        `openid:${APPLE_ISSUER}:email`,
        `openid:${MICROSOFT_ISSUER}:verified_email`,
        `sso:${SSO_DOMAIN}:name`,
        `sso:${SSO_DOMAIN}:email`,
      ];
      const groups = resolveAttributeGroups(requestedKeys, fullyAvailable);
      const allOptions = groups.flatMap((g) => g.options);
      expect(allOptions.length).toBeGreaterThan(0);
      for (const option of allOptions) {
        expect(option.key).toMatch(/^(openid|sso):/);
        // At least two colons (scope-prefix + name separator) so the
        // last-colon split actually carves out a scope.
        expect(option.key.split(":").length).toBeGreaterThanOrEqual(3);
      }
    });
  });

  describe("value decoding", () => {
    it("decodes UTF-8 bytes into `displayValue` and preserves the raw bytes", () => {
      const utf8 = bytes("Zürich naïve résumé");
      const groups = resolveAttributeGroups(
        [`openid:${GOOGLE_ISSUER}:name`],
        [[`openid:${GOOGLE_ISSUER}:name`, utf8]],
      );
      expect(groups[0].options[0].displayValue).toBe("Zürich naïve résumé");
      expect(arrayOf(groups[0].options[0].rawValue)).toEqual(arrayOf(utf8));
    });

    it("accepts `number[]` raw values (the Candid wire shape) as well as Uint8Array", () => {
      const utf8 = bytes("plain");
      const groups = resolveAttributeGroups(
        [`openid:${GOOGLE_ISSUER}:name`],
        [[`openid:${GOOGLE_ISSUER}:name`, Array.from(utf8)]],
      );
      expect(groups[0].options[0].displayValue).toBe("plain");
      expect(arrayOf(groups[0].options[0].rawValue)).toEqual(arrayOf(utf8));
    });
  });
});
