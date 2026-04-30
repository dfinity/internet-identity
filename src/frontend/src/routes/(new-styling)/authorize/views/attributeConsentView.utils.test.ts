import type {
  AttributeGroup,
  AvailableAttribute,
} from "$lib/stores/attributeConsent.store";
import { resolveAttributeGroups } from "$lib/stores/channelHandlers/attributes";
import { groupId, mergeGroups } from "./attributeConsentView.utils";

const GOOGLE = "https://accounts.google.com";
const APPLE = "https://appleid.apple.com";
const MICROSOFT = "https://login.microsoftonline.com/{tid}/v2.0";
const SSO = "dfinity.org";

const encoder = new TextEncoder();
const bytes = (s: string): Uint8Array => encoder.encode(s);

const option = (
  key: string,
  value: string,
  omitScope: boolean,
): AvailableAttribute => ({
  key,
  displayValue: value,
  rawValue: bytes(value),
  omitScope,
});

/**
 * Compact projection of a `MergedGroup[]` for assertions: each row's
 * name, form, displayed values per option, and the keys that originals
 * will certify. Lets the assertion read like the spec table.
 */
type RowShape = {
  name: string;
  omitScope: boolean;
  options: { display: string; certifies: string[] }[];
};
const shape = (groups: ReturnType<typeof mergeGroups>): RowShape[] =>
  groups.map((g) => ({
    name: g.name,
    omitScope: g.omitScope,
    options: g.options.map((o) => ({
      display: o.display.displayValue,
      certifies: o.originals.map((og) => og.key).sort(),
    })),
  }));

describe("mergeGroups — request bucketing", () => {
  it("returns one row per requested key in request order", () => {
    const groups = resolveAttributeGroups(
      ["email", "name", `sso:${SSO}:name`],
      [
        [`openid:${GOOGLE}:name`, bytes("Google Name")],
        [`openid:${GOOGLE}:email`, bytes("user@google.com")],
        [`sso:${SSO}:name`, bytes("SSO Name")],
      ],
    );
    expect(mergeGroups(groups).map((g) => `${g.name}:${g.omitScope}`)).toEqual([
      "email:true",
      "name:true",
      "name:false",
    ]);
  });

  it("keeps distinct scoped requests as separate rows (no merging across issuers)", () => {
    const groups = resolveAttributeGroups(
      [
        `openid:${GOOGLE}:name`,
        `openid:${APPLE}:name`,
        `openid:${MICROSOFT}:name`,
      ],
      [
        [`openid:${GOOGLE}:name`, bytes("Google Name")],
        [`openid:${APPLE}:name`, bytes("Apple Name")],
        [`openid:${MICROSOFT}:name`, bytes("Microsoft Name")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "name",
        omitScope: false,
        options: [
          { display: "Google Name", certifies: [`openid:${GOOGLE}:name`] },
        ],
      },
      {
        name: "name",
        omitScope: false,
        options: [
          { display: "Apple Name", certifies: [`openid:${APPLE}:name`] },
        ],
      },
      {
        name: "name",
        omitScope: false,
        options: [
          {
            display: "Microsoft Name",
            certifies: [`openid:${MICROSOFT}:name`],
          },
        ],
      },
    ]);
  });

  it("dedupes identical duplicate scoped requests so the canister only sees one spec", () => {
    // A misbehaving dapp asks for the same scoped key twice. Sending
    // both as separate specs trips the canister's "duplicate attribute"
    // check, so the second occurrence is dropped at bucketize time.
    const dupKey = `openid:${GOOGLE}:name`;
    const groups: AttributeGroup[] = [
      { name: "name", options: [option(dupKey, "Google Name", false)] },
      { name: "name", options: [option(dupKey, "Google Name", false)] },
    ];
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "name",
        omitScope: false,
        options: [{ display: "Google Name", certifies: [dupKey] }],
      },
    ]);
  });

  it("dedupes identical duplicate unscoped requests across every fan-out scope", () => {
    // The dapp asks for the same unscoped `email` twice. Without
    // dedup, every scope the fan-out hits would carry two identical
    // originals (same key + omit_scope: true), and the canister would
    // reject the duplicates. Phase 3 collapses identical values across
    // scopes too, so the final shape ends up at a single spec.
    const groups = resolveAttributeGroups(
      ["email", "email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@dfinity.org")],
        [`sso:${SSO}:email`, bytes("a@dfinity.org")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: true,
        options: [
          {
            display: "a@dfinity.org",
            certifies: [`openid:${GOOGLE}:email`],
          },
        ],
      },
    ]);
  });
});

describe("mergeGroups — email + verified_email per (form, scope)", () => {
  // The five numbered cases from the spec.

  it("scoped, both available → merges into one option, certifies both", () => {
    // openid:google:email(a@gmail.com) + openid:google:verified_email(a@gmail.com)
    // → Email: a@gmail.com (certifies both)
    const groups = resolveAttributeGroups(
      [`openid:${GOOGLE}:email`, `openid:${GOOGLE}:verified_email`],
      [
        [`openid:${GOOGLE}:email`, bytes("a@gmail.com")],
        [`openid:${GOOGLE}:verified_email`, bytes("a@gmail.com")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: false,
        options: [
          {
            display: "a@gmail.com",
            certifies: [
              `openid:${GOOGLE}:email`,
              `openid:${GOOGLE}:verified_email`,
            ],
          },
        ],
      },
    ]);
  });

  it("scoped, only email available → email shown, certifies only email", () => {
    // openid:google:email(a@gmail.com) + openid:google:verified_email(-)
    // → Email: a@gmail.com (certifies only email)
    const groups = resolveAttributeGroups(
      [`openid:${GOOGLE}:email`, `openid:${GOOGLE}:verified_email`],
      [[`openid:${GOOGLE}:email`, bytes("a@gmail.com")]],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: false,
        options: [
          {
            display: "a@gmail.com",
            certifies: [`openid:${GOOGLE}:email`],
          },
        ],
      },
    ]);
  });

  it("unscoped, partial verified availability → email-only scopes drop, intersection survives", () => {
    // email(a@gmail.com, b@gmail.com) + verified_email(a@gmail.com)
    // → Email: a@gmail.com (certifies both)
    // (b is dropped because verified_email is requested but not available
    //  for b's scope.)
    const groups = resolveAttributeGroups(
      ["email", "verified_email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@gmail.com")],
        [`openid:${GOOGLE}:verified_email`, bytes("a@gmail.com")],
        [`openid:${APPLE}:email`, bytes("b@gmail.com")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: true,
        options: [
          {
            display: "a@gmail.com",
            certifies: [
              `openid:${GOOGLE}:email`,
              `openid:${GOOGLE}:verified_email`,
            ],
          },
        ],
      },
    ]);
  });

  it("unscoped, full verified availability → every option merged, certifies both per scope", () => {
    // email(a@gmail.com, b@gmail.com, c@outlook.com) + verified_email(...)
    // → Email: a, b, c (certifies both per scope)
    const groups = resolveAttributeGroups(
      ["email", "verified_email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@gmail.com")],
        [`openid:${GOOGLE}:verified_email`, bytes("a@gmail.com")],
        [`openid:${APPLE}:email`, bytes("b@gmail.com")],
        [`openid:${APPLE}:verified_email`, bytes("b@gmail.com")],
        [`openid:${MICROSOFT}:email`, bytes("c@outlook.com")],
        [`openid:${MICROSOFT}:verified_email`, bytes("c@outlook.com")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: true,
        options: [
          {
            display: "a@gmail.com",
            certifies: [
              `openid:${GOOGLE}:email`,
              `openid:${GOOGLE}:verified_email`,
            ],
          },
          {
            display: "b@gmail.com",
            certifies: [
              `openid:${APPLE}:email`,
              `openid:${APPLE}:verified_email`,
            ],
          },
          {
            display: "c@outlook.com",
            certifies: [
              `openid:${MICROSOFT}:email`,
              `openid:${MICROSOFT}:verified_email`,
            ],
          },
        ],
      },
    ]);
  });

  it("unscoped, verified requested but nothing available → email shown intact, certifies only email", () => {
    // email(a@gmail.com, b@gmail.com) + verified_email(-)
    // → Email: a, b (certifies only email)
    const groups = resolveAttributeGroups(
      ["email", "verified_email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@gmail.com")],
        [`openid:${APPLE}:email`, bytes("b@gmail.com")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: true,
        options: [
          {
            display: "a@gmail.com",
            certifies: [`openid:${GOOGLE}:email`],
          },
          {
            display: "b@gmail.com",
            certifies: [`openid:${APPLE}:email`],
          },
        ],
      },
    ]);
  });

  // Additional coverage of merge edges the numbered spec didn't include.

  it("verified-only scopes (no matching email scope) stay in their own row", () => {
    // email(google) + verified_email(google, apple) → email row: google
    // (folded), verified row: apple (leftover).
    const groups = resolveAttributeGroups(
      ["email", "verified_email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@gmail.com")],
        [`openid:${GOOGLE}:verified_email`, bytes("a@gmail.com")],
        [`openid:${APPLE}:verified_email`, bytes("b@gmail.com")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: true,
        options: [
          {
            display: "a@gmail.com",
            certifies: [
              `openid:${GOOGLE}:email`,
              `openid:${GOOGLE}:verified_email`,
            ],
          },
        ],
      },
      {
        name: "verified_email",
        omitScope: true,
        options: [
          {
            display: "b@gmail.com",
            certifies: [`openid:${APPLE}:verified_email`],
          },
        ],
      },
    ]);
  });

  it("disjoint scopes → no intersection, no merge, both rows render", () => {
    // email returns google only; verified_email returns apple only.
    // No (form, scope) pair matches so each request keeps its own row.
    const groups = resolveAttributeGroups(
      ["email", "verified_email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@gmail.com")],
        [`openid:${APPLE}:verified_email`, bytes("b@gmail.com")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: true,
        options: [
          {
            display: "a@gmail.com",
            certifies: [`openid:${GOOGLE}:email`],
          },
        ],
      },
      {
        name: "verified_email",
        omitScope: true,
        options: [
          {
            display: "b@gmail.com",
            certifies: [`openid:${APPLE}:verified_email`],
          },
        ],
      },
    ]);
  });

  it("mixed forms (scoped email + unscoped verified_email) don't merge", () => {
    const groups = resolveAttributeGroups(
      [`openid:${GOOGLE}:email`, "verified_email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@gmail.com")],
        [`openid:${GOOGLE}:verified_email`, bytes("a@gmail.com")],
      ],
    );
    expect(mergeGroups(groups).map((g) => `${g.name}:${g.omitScope}`)).toEqual([
      "email:false",
      "verified_email:true",
    ]);
  });

  it("sole verified_email request stays as its own row", () => {
    const groups = resolveAttributeGroups(
      ["verified_email"],
      [[`openid:${GOOGLE}:verified_email`, bytes("a@gmail.com")]],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "verified_email",
        omitScope: true,
        options: [
          {
            display: "a@gmail.com",
            certifies: [`openid:${GOOGLE}:verified_email`],
          },
        ],
      },
    ]);
  });
});

describe("mergeGroups — identical-value collapse for unscoped fan-outs", () => {
  it("collapses an unscoped fan-out whose every scope returns the same value", () => {
    // The same email is published under three different scopes — picking
    // between three identical values isn't a meaningful choice, so the
    // row collapses to one option. Only the first scope's original is
    // certified: every dropped option would have produced the same
    // canister output (`email = a@dfinity.org` with `omit_scope: true`),
    // so sending all three would just be redundant work the user never
    // implicitly approved (they only saw one row).
    const groups = resolveAttributeGroups(
      ["email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@dfinity.org")],
        [`openid:${APPLE}:email`, bytes("a@dfinity.org")],
        [`sso:${SSO}:email`, bytes("a@dfinity.org")],
      ],
    );
    expect(shape(mergeGroups(groups))).toEqual([
      {
        name: "email",
        omitScope: true,
        options: [
          {
            display: "a@dfinity.org",
            certifies: [`openid:${GOOGLE}:email`],
          },
        ],
      },
    ]);
  });

  it("keeps every option when even one value differs", () => {
    const groups = resolveAttributeGroups(
      ["email"],
      [
        [`openid:${GOOGLE}:email`, bytes("a@dfinity.org")],
        [`openid:${APPLE}:email`, bytes("a@dfinity.org")],
        [`sso:${SSO}:email`, bytes("different@dfinity.org")],
      ],
    );
    expect(
      mergeGroups(groups)[0].options.map((o) => o.display.displayValue),
    ).toEqual(["a@dfinity.org", "a@dfinity.org", "different@dfinity.org"]);
  });

  it("doesn't collapse scoped rows (always single-option, no picker to dedupe)", () => {
    const groups = resolveAttributeGroups(
      [`openid:${GOOGLE}:email`, `openid:${APPLE}:email`],
      [
        [`openid:${GOOGLE}:email`, bytes("same@dfinity.org")],
        [`openid:${APPLE}:email`, bytes("same@dfinity.org")],
      ],
    );
    // Two distinct scoped requests stay as two distinct rows even though
    // they carry the same value.
    expect(mergeGroups(groups)).toHaveLength(2);
  });
});

describe("groupId", () => {
  it("returns `name:u` for unscoped fan-out groups", () => {
    expect(
      groupId({
        name: "email",
        omitScope: true,
        options: [
          {
            display: option(`openid:${GOOGLE}:email`, "x", true),
            originals: [],
          },
        ],
      }),
    ).toBe("email:u");
  });

  it("keys scoped groups by scope so distinct issuers get distinct ids", () => {
    expect(
      groupId({
        name: "name",
        omitScope: false,
        options: [
          {
            display: option(`openid:${GOOGLE}:name`, "x", false),
            originals: [],
          },
        ],
      }),
    ).toBe(`name:s:openid:${GOOGLE}`);
    expect(
      groupId({
        name: "name",
        omitScope: false,
        options: [
          {
            display: option(`openid:${APPLE}:name`, "x", false),
            originals: [],
          },
        ],
      }),
    ).toBe(`name:s:openid:${APPLE}`);
  });

  it("stays stable across email/verified_email merging (uses scope, not the swapped display key)", () => {
    // After merge, a scoped email row's `display.key` may be the
    // verified_email key — but `groupId` keys by scope so the row's
    // identity in the selections map doesn't shift mid-flight.
    expect(
      groupId({
        name: "email",
        omitScope: false,
        options: [
          {
            display: option(`openid:${GOOGLE}:verified_email`, "x", false),
            originals: [],
          },
        ],
      }),
    ).toBe(`email:s:openid:${GOOGLE}`);
  });
});
