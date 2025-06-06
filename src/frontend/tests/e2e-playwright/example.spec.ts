import { test, expect, BrowserContext, Page } from "@playwright/test";
import { borcDist } from "./borc.dist";

interface MockedPasskey {
  mockCreate: (page: Page) => Promise<void>;
}

const mockDiscoverablePasskey = async (): Promise<MockedPasskey> => {
  const keyPair = await crypto.subtle.generateKey(
    {
      name: "ECDSA",
      namedCurve: "P-256",
    },
    true,
    ["sign"],
  );
  const credentialId = crypto.getRandomValues(new Uint8Array(16));
  const privateJwk = await crypto.subtle.exportKey("jwk", keyPair.privateKey);
  const publicJwk = await crypto.subtle.exportKey("jwk", keyPair.publicKey);
  return {
    async mockCreate(page) {
      await page.evaluate(borcDist);
      await page.evaluate(
        async ([credentialId, privateJwk, publicJwk]) => {
          // // @ts-ignore
          // const cbor = await import("https://esm.sh/cbor?bundle");
          //
          // function encodeUint(num: number): Uint8Array {
          //   if (num < 24) return Uint8Array.of(num);
          //   if (num < 256) return Uint8Array.of(0x18, num);
          //   if (num < 65536) return Uint8Array.of(0x19, num >> 8, num & 0xff);
          //   if (num < 0x100000000)
          //     return Uint8Array.of(
          //       0x1a,
          //       (num >>> 24) & 0xff,
          //       (num >>> 16) & 0xff,
          //       (num >>> 8) & 0xff,
          //       num & 0xff,
          //     );
          //   throw new Error("encodeUint supports up to 2^32 - 1");
          // }
          //
          // function encodeInt(num: number): Uint8Array {
          //   if (num >= 0) return encodeUint(num);
          //   const positive = -1 - num;
          //   if (positive < 24) return Uint8Array.of(0x20 + positive);
          //   if (positive < 256) return Uint8Array.of(0x38, positive);
          //   if (positive < 65536)
          //     return Uint8Array.of(0x39, positive >> 8, positive & 0xff);
          //   if (positive < 0x100000000)
          //     return Uint8Array.of(
          //       0x3a,
          //       (positive >>> 24) & 0xff,
          //       (positive >>> 16) & 0xff,
          //       (positive >>> 8) & 0xff,
          //       positive & 0xff,
          //     );
          //   throw new Error("encodeInt supports down to -2^32");
          // }
          //
          // // Helper: encode byte string in CBOR
          // function encodeByteString(bytes: Uint8Array) {
          //   const len = bytes.length;
          //   let header;
          //   if (len < 24) header = Uint8Array.of(0x40 + len);
          //   else if (len < 256) header = Uint8Array.of(0x58, len);
          //   else throw new Error("Byte string too long");
          //
          //   return concatUint8Arrays(header, bytes);
          // }
          //
          // // Helper: encode text string in CBOR
          // function encodeTextString(str: string) {
          //   const utf8 = new TextEncoder().encode(str);
          //   const len = utf8.length;
          //   let header;
          //   if (len < 24) header = Uint8Array.of(0x60 + len);
          //   else if (len < 256) header = Uint8Array.of(0x78, len);
          //   else throw new Error("Text string too long");
          //   return concatUint8Arrays(header, utf8);
          // }
          //
          // function encodeArray(arr: unknown[]): Uint8Array {
          //   const len = arr.length;
          //   let header: Uint8Array;
          //   if (len < 24) header = Uint8Array.of(0x80 + len);
          //   else if (len < 256) header = Uint8Array.of(0x98, len);
          //   else throw new Error("Array too long");
          //
          //   const items = arr.map(encodeValue);
          //   return concatUint8Arrays(header, ...items);
          // }
          //
          // function encodeValue(val: unknown): Uint8Array {
          //   if (val instanceof Uint8Array) return encodeByteString(val);
          //   if (typeof val === "string") return encodeTextString(val);
          //   if (typeof val === "number")
          //     return val >= 0 ? encodeUint(val) : encodeInt(val);
          //   if (Array.isArray(val)) return encodeArray(val);
          //   if (typeof val === "object" && val !== null)
          //     return encodeMap(val as Record<string, unknown>);
          //   throw new Error("Unsupported value type: " + typeof val);
          // }
          //
          // // Helper: encode map with integer keys (assuming small number of entries)
          // function encodeMap(mapObj: Record<string | number, unknown>) {
          //   const keys = Object.keys(mapObj);
          //   const len = keys.length;
          //   let header;
          //   if (len < 24) header = Uint8Array.of(0xa0 + len);
          //   else if (len < 256) header = Uint8Array.of(0xb8, len);
          //   else throw new Error("Map too big");
          //
          //   let content = new Uint8Array(0);
          //   for (const key of keys) {
          //     // keys are integers (possibly negative)
          //     const keyNum = Number(key);
          //     const encodedKey = isNaN(keyNum)
          //       ? encodeTextString(key)
          //       : keyNum >= 0
          //         ? encodeUint(keyNum)
          //         : encodeInt(keyNum);
          //     const val = mapObj[key];
          //
          //     const encodedVal = encodeValue(val);
          //     content = concatUint8Arrays(content, encodedKey, encodedVal);
          //   }
          //   return concatUint8Arrays(header, content);
          // }
          //
          // // Helper: concat multiple Uint8Arrays
          // function concatUint8Arrays(...arrays: Uint8Array[]) {
          //   let totalLength = 0;
          //   for (const arr of arrays) totalLength += arr.length;
          //   const result = new Uint8Array(totalLength);
          //   let offset = 0;
          //   for (const arr of arrays) {
          //     result.set(arr, offset);
          //     offset += arr.length;
          //   }
          //   return result;
          // }

          const privateKey = await crypto.subtle.importKey(
            "jwk",
            privateJwk,
            {
              name: "ECDSA",
              namedCurve: "P-256",
            },
            true,
            ["sign"],
          );
          const create = window.navigator.credentials.create;
          navigator.credentials.create = async (options) => {
            const clientData = {
              type: "webauthn.create",
              challenge: btoa(
                String.fromCharCode(
                  ...new Uint8Array(
                    options?.publicKey?.challenge as ArrayBuffer,
                  ),
                ),
              ),
              origin: options?.publicKey?.rp.id ?? location.origin,
              crossOrigin: false,
            };
            const clientDataJSON = new TextEncoder().encode(
              JSON.stringify(clientData),
            );
            const rpIdHash = new Uint8Array(
              await crypto.subtle.digest(
                "SHA-256",
                new TextEncoder().encode(
                  options?.publicKey?.rp.id ?? location.origin,
                ),
              ),
            );
            const flags = new Uint8Array([0x01]);
            const signCount = new Uint8Array([0x00, 0x00, 0x00, 0x01]);
            const mockCredentialData = crypto.getRandomValues(
              new Uint8Array(64),
            );
            const authenticatorData = new Uint8Array([
              ...rpIdHash,
              ...flags,
              ...signCount,
              ...mockCredentialData,
            ]);
            const signature = await crypto.subtle.sign(
              { name: "ECDSA", hash: "SHA-256" },
              privateKey,
              authenticatorData,
            );
            // @ts-expect-error ignore
            const attestationObject = Borc.encode({
              fmt: "packed",
              attStmt: {
                alg: -7,
                sig: new Uint8Array(signature),
              },
              authData: authenticatorData,
            });
            console.warn(
              "hex",
              [...attestationObject]
                .map((b) => b.toString(16).padStart(2, "0"))
                .join(""),
            );
            // @ts-expect-error ignore
            const coseKey = Borc.encode({
              [1]: 2, // kty: EC
              [3]: -7, // alg: ES256
              [-1]: 1, // crv: P-256
              [-2]: Uint8Array.from(
                atob(
                  publicJwk
                    .x!.replace(/-/g, "+")
                    .replace(/_/g, "/")
                    .padEnd(Math.ceil(publicJwk.x!.length / 4) * 4, "="),
                ),
                (m) => m.charCodeAt(0),
              ),
              [-3]: Uint8Array.from(
                atob(
                  publicJwk
                    .y!.replace(/-/g, "+")
                    .replace(/_/g, "/")
                    .padEnd(Math.ceil(publicJwk.x!.length / 4) * 4, "="),
                ),
                (m) => m.charCodeAt(0),
              ),
            });
            const response: AuthenticatorAttestationResponse = {
              clientDataJSON,
              attestationObject,
              getAuthenticatorData() {
                return authenticatorData.buffer;
              },
              getPublicKey() {
                return coseKey;
              },
              getPublicKeyAlgorithm() {
                return -7;
              },
              getTransports() {
                return ["usb"];
              },
            };
            const credential: PublicKeyCredential = {
              authenticatorAttachment: null,
              rawId: credentialId.buffer,
              id: btoa(String.fromCharCode(...credentialId))
                .replace(/\+/g, "-")
                .replace(/\//g, "_")
                .replace(/=+$/, ""),
              type: "public-key",
              response,
              toJSON() {
                throw new Error("Not implemented");
              },
              getClientExtensionResults() {
                throw new Error("Not implemented");
              },
            };
            navigator.credentials.create = create;
            return credential;
          };
        },
        [credentialId, privateJwk, publicJwk] as const,
      );
    },
  };
};

test("register with a new passkey", async ({ page, context }) => {
  const passkey = await mockDiscoverablePasskey();
  await page.goto("https://nice-name.com");
  await page
    .getByRole("textbox", { name: "Identity Provider" })
    .fill(
      "https://identity.internetcomputer.org?feature_flag_discoverable_passkey_flow=true",
    );
  await expect(page.locator("#principal")).toBeHidden();
  const pagePromise = context.waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const newPage = await pagePromise;
  await passkey.mockCreate(newPage);
  await newPage.getByRole("button", { name: "Continue with Passkey" }).click();
  await newPage.getByRole("button", { name: "Set up a new Passkey" }).click();
  await newPage.getByLabel("Identity name").fill("John Doe");
  await newPage.getByRole("button", { name: "Create Passkey" }).click();
  await newPage.waitForEvent("close");
  await expect(page.locator("#principal")).not.toBeEmpty();
});
