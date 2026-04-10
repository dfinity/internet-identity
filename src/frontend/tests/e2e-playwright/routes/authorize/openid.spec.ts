import { expect } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";
import { fromBase64 } from "../../utils";

type Icrc3Value =
  | { Nat: bigint }
  | { Int: bigint }
  | { Blob: number[] }
  | { Text: string }
  | { Array: Icrc3Value[] }
  | { Map: [string, Icrc3Value][] };

const Icrc3Value = IDL.Rec();
Icrc3Value.fill(
  IDL.Variant({
    Nat: IDL.Nat,
    Int: IDL.Int,
    Blob: IDL.Vec(IDL.Nat8),
    Text: IDL.Text,
    Array: IDL.Vec(Icrc3Value),
    Map: IDL.Vec(IDL.Tuple(IDL.Text, Icrc3Value)),
  }),
);

test.describe("Authorize with direct OpenID", () => {
  test.describe("with name and email attributes", () => {
    const name = "John Doe";
    const email = "john.doe@example.com";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [
          {
            claims: { name, email },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: [
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:name`,
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:email`,
        ],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) {
        return;
      }

      expect(authorizedIcrc3Attributes.signature.length).toBeGreaterThan(0);

      // Decode the Candid-encoded ICRC-3 Value map.
      const dataBytes = fromBase64(authorizedIcrc3Attributes.data);
      const { Map: map } = IDL.decode([Icrc3Value], dataBytes)[0] as {
        Map: [string, Icrc3Value][];
      };
      const blobEntries = Object.fromEntries(
        map
          .filter(
            (entry): entry is [string, { Blob: number[] }] =>
              "Blob" in entry[1],
          )
          .map(([key, { Blob: blob }]) => [
            key,
            new TextDecoder().decode(new Uint8Array(blob)),
          ]),
      );

      expect(blobEntries).toMatchObject({
        [`openid:http://localhost:${DEFAULT_OPENID_PORT}:name`]: name,
        [`openid:http://localhost:${DEFAULT_OPENID_PORT}:email`]: email,
      });
    });

    test("should return ICRC-3 attributes", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });
});
