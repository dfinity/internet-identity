import { expect } from "@playwright/test";
import { test } from "../fixtures";
import { addVirtualAuthenticator, II_URL } from "../utils";
import fs from "fs";

const SELF_SERVICE_URL = II_URL + "/self-service";

test.describe("Self-service passkey debugging", () => {
  test("Shows passkey test result after clicking 'Test passkey support'", async ({
    page,
  }) => {
    await addVirtualAuthenticator(page);
    await page.goto(SELF_SERVICE_URL);

    await expect(page.getByText("No passkey tests yet")).toBeVisible();
    await page.getByRole("button", { name: "Test passkey support" }).click();

    // Placeholder disappears and a result card appears
    await expect(page.getByText("No passkey tests yet")).toBeHidden();
    // Virtual authenticator has no AAGUID → provider shown as "Unknown"
    await expect(page.getByText("Unknown").first()).toBeVisible();
  });

  test("Multiple test runs each produce a result card", async ({ page }) => {
    await addVirtualAuthenticator(page);
    await page.goto(SELF_SERVICE_URL);

    await page.getByRole("button", { name: "Test passkey support" }).click();
    await expect(page.getByText("No passkey tests yet")).toBeHidden();

    await page.getByRole("button", { name: "Test passkey support" }).click();

    // Two result cards should be visible (the component renders them in a list)
    const resultCards = page.locator(
      ".border-border-secondary.bg-bg-tertiary.rounded-xl",
    );
    await expect(resultCards).toHaveCount(2);
  });

  test("Exported JSON contains all required debug fields", async ({ page }) => {
    await addVirtualAuthenticator(page);
    await page.goto(SELF_SERVICE_URL);

    await page.getByRole("button", { name: "Test passkey support" }).click();
    await expect(page.getByText("No passkey tests yet")).toBeHidden();

    const downloadPromise = page.waitForEvent("download");
    await page.getByRole("button", { name: "Export JSON" }).click();
    const download = await downloadPromise;

    // Filename should match `<hostname>-<timestamp>-self-service.json`
    expect(download.suggestedFilename()).toMatch(/^.+-\d+-self-service\.json$/);

    const filePath = await download.path();
    const json = JSON.parse(fs.readFileSync(filePath!, "utf-8")) as {
      origin: string;
      date: number;
      identities: unknown[];
      tests: Array<{
        aaguid?: string;
        date: number;
        debug?: {
          credentialIdHex: string;
          rawCoseHex: string;
          cleanedCoseHex: string;
          derHex: string;
          allEntries: Array<{ key: number | string; valueCborHex: string }>;
          filteredEntries: Array<{ key: number | string; valueCborHex: string }>;
          icCall?: {
            senderPubkeyHex: string;
            delegationPubkeyHex: string;
            callerPrincipal?: string;
            error?: string;
          };
        };
      }>;
    };

    // Top-level shape
    expect(json.origin).toMatch(/^https?:\/\//);
    expect(typeof json.date).toBe("number");
    expect(Array.isArray(json.identities)).toBe(true);
    expect(json.tests).toHaveLength(1);

    const { debug } = json.tests[0];
    expect(debug).toBeDefined();

    // All COSE debug fields must be present and non-empty hex strings
    expect(debug!.credentialIdHex).toMatch(/^[0-9a-f]+$/);
    expect(debug!.rawCoseHex).toMatch(/^[0-9a-f]+$/);
    expect(debug!.cleanedCoseHex).toMatch(/^[0-9a-f]+$/);
    expect(debug!.derHex).toMatch(/^[0-9a-f]+$/);

    // Entry arrays must be present
    expect(Array.isArray(debug!.allEntries)).toBe(true);
    expect(Array.isArray(debug!.filteredEntries)).toBe(true);

    // filteredEntries must be a subset of allEntries
    expect(debug!.filteredEntries.length).toBeLessThanOrEqual(
      debug!.allEntries.length,
    );
  });

  test("IC call succeeds — passkey COSE key is accepted by the IC", async ({
    page,
  }) => {
    await addVirtualAuthenticator(page);
    await page.goto(SELF_SERVICE_URL);

    await page.getByRole("button", { name: "Test passkey support" }).click();
    await expect(page.getByText("No passkey tests yet")).toBeHidden();

    const downloadPromise = page.waitForEvent("download");
    await page.getByRole("button", { name: "Export JSON" }).click();
    const download = await downloadPromise;

    const filePath = await download.path();
    const json = JSON.parse(fs.readFileSync(filePath!, "utf-8")) as {
      tests: Array<{
        debug?: {
          icCall?: {
            senderPubkeyHex: string;
            delegationPubkeyHex: string;
            callerPrincipal?: string;
            error?: string;
          };
        };
      }>;
    };

    const icCall = json.tests[0]?.debug?.icCall;
    expect(icCall).toBeDefined();

    // The IC must return a valid principal — not an error.
    // This proves the COSE key produced by the virtual authenticator passes
    // the IC's COSE parser (no unexpected entries such as key_ops).
    expect(icCall!.callerPrincipal).toBeTruthy();
    expect(icCall!.error).toBeUndefined();

    // Hex keys must be present (used for offline debugging if the IC rejects the key)
    expect(icCall!.senderPubkeyHex).toMatch(/^[0-9a-f]+$/);
    expect(icCall!.delegationPubkeyHex).toMatch(/^[0-9a-f]+$/);
  });

  test("COSE filteredEntries only contain the standard IC-accepted key parameters", async ({
    page,
  }) => {
    // Standard COSE key parameters the IC accepts: kty=1, alg=3, crv=-1, x=-2, y=-3
    const STANDARD_COSE_PARAMS = new Set([1, 3, -1, -2, -3]);

    await addVirtualAuthenticator(page);
    await page.goto(SELF_SERVICE_URL);

    await page.getByRole("button", { name: "Test passkey support" }).click();
    await expect(page.getByText("No passkey tests yet")).toBeHidden();

    const downloadPromise = page.waitForEvent("download");
    await page.getByRole("button", { name: "Export JSON" }).click();
    const download = await downloadPromise;

    const filePath = await download.path();
    const json = JSON.parse(fs.readFileSync(filePath!, "utf-8")) as {
      tests: Array<{
        debug?: {
          filteredEntries: Array<{ key: number | string; valueCborHex: string }>;
          allEntries: Array<{ key: number | string; valueCborHex: string }>;
        };
      }>;
    };

    const { filteredEntries, allEntries } = json.tests[0].debug!;

    // Every filtered entry must be one of the 5 standard COSE key parameters
    for (const entry of filteredEntries) {
      expect(STANDARD_COSE_PARAMS.has(entry.key as number)).toBe(true);
    }

    // A P-256 key must have all 5 standard entries
    const filteredKeys = filteredEntries.map((e) => e.key);
    expect(filteredKeys).toEqual(expect.arrayContaining([1, 3, -1, -2, -3]));

    // allEntries must include everything in filteredEntries
    const allKeys = new Set(allEntries.map((e) => e.key));
    for (const key of filteredKeys) {
      expect(allKeys.has(key)).toBe(true);
    }
  });
});
