import { expect, type Page } from "@playwright/test";
import { test } from "../../fixtures";
import { holdToConfirm, II_URL } from "../../utils";
import type { McpFixture } from "../../fixtures/mcp";

/** Set in `src/internet_identity/local_test_arg.did` (and mirrored in
 *  `scripts/dev-e2e-setup`'s inline install args) as `mcp_official_url`.
 *  Deliberately not the mcp fixture's `mcp.id.ai` origin: that stands in for
 *  an untrusted/custom server elsewhere, and reusing it here would make the
 *  fixture's fake server "official" everywhere, breaking those tests. */
const OFFICIAL_URL = "https://official-mcp.id.ai/mcp";

const waitForConfigWrite = (page: Page) =>
  page.waitForResponse(
    (response) =>
      response.url().includes("/call") &&
      response.request().method() === "POST",
  );

const toggleAiAccess = (page: Page) =>
  Promise.all([
    waitForConfigWrite(page),
    page.getByRole("switch", { name: "AI access" }).click(),
  ]);

/** Opens the customize dialog and confirms `mcp.mcpOrigin` as the custom
 *  connector URL, mocking its RFC 9728 metadata so the probe verifies fast
 *  and clean (activation doesn't depend on the probe outcome). */
const addCustomConnector = async (
  page: Page,
  mcp: McpFixture,
): Promise<string> => {
  await page.getByRole("button", { name: "Customize" }).click();
  await expect(
    page.getByRole("heading", { name: "Customize AI access" }),
  ).toBeVisible();

  await page.route(
    `${mcp.mcpOrigin}/.well-known/oauth-protected-resource**`,
    (route) =>
      route.fulfill({
        status: 200,
        headers: {
          "access-control-allow-origin": "*",
          "content-type": "application/json",
        },
        body: JSON.stringify({
          authorization_servers: [mcp.mcpOrigin],
          resource: `${mcp.mcpOrigin}/mcp`,
        }),
      }),
  );

  const customUrl = `${mcp.mcpOrigin}/mcp`;
  await page.getByLabel("MCP server URL").fill(customUrl);
  await holdToConfirm(page, "Hold to continue");
  await expect(
    page.getByRole("button", { name: "Restore default" }),
  ).toBeVisible();
  return customUrl;
};

test.describe("MCP settings", () => {
  test.beforeEach(async ({ page, identities, signInWithIdentity }) => {
    await page.goto(II_URL + "/manage/settings");
    await signInWithIdentity(page, identities[0].identityNumber);
    await expect(
      page.getByRole("heading", { name: "AI access" }),
    ).toBeVisible();
  });

  test("shows the official connector once AI access is enabled", async ({
    page,
  }) => {
    // Enabling with no custom URL set falls back to the configured official
    // connector rather than opening the add dialog.
    await toggleAiAccess(page);

    await expect(page.getByText("Internet Computer MCP")).toBeVisible();
    await expect(page.getByText("Official · Hosted by DFINITY")).toBeVisible();
    await expect(page.getByText(OFFICIAL_URL)).toBeVisible();
    await expect(page.getByRole("button", { name: "Customize" })).toBeVisible();
  });

  test("refuses the official connector as a custom one", async ({ page }) => {
    await toggleAiAccess(page);
    await page.getByRole("button", { name: "Customize" }).click();
    await page.getByLabel("MCP server URL").fill(OFFICIAL_URL);

    // It is already the default, so spending the one custom slot on it is
    // refused rather than silently accepted.
    await expect(page.getByText(/already your default/)).toBeVisible();
    await expect(
      page.getByRole("button", { name: "Hold to continue" }),
    ).toBeDisabled();
  });

  test("a custom connector replaces the official one", async ({
    page,
    mcp,
  }) => {
    await toggleAiAccess(page);
    const customUrl = await addCustomConnector(page, mcp);

    await expect(page.getByText("mcp.id.ai", { exact: true })).toBeVisible();
    await expect(
      page.getByText("Added by you · Replaces the official connector"),
    ).toBeVisible();
    await expect(page.getByText(customUrl)).toBeVisible();
    // The official connector is no longer shown while the custom one is active.
    await expect(page.getByText("Internet Computer MCP")).toBeHidden();
  });

  test("restoring default returns to the official connector", async ({
    page,
    mcp,
  }) => {
    await toggleAiAccess(page);
    await addCustomConnector(page, mcp);

    await Promise.all([
      waitForConfigWrite(page),
      page.getByRole("button", { name: "Restore default" }).click(),
    ]);

    await expect(page.getByText("Internet Computer MCP")).toBeVisible();
    await expect(page.getByText("Official · Hosted by DFINITY")).toBeVisible();
    await expect(page.getByText(OFFICIAL_URL)).toBeVisible();
    await expect(page.getByRole("button", { name: "Customize" })).toBeVisible();
    await expect(page.getByText("mcp.id.ai", { exact: true })).toBeHidden();
  });

  test("disabling AI access hides the connector, and re-enabling returns to the official connector (not the previous custom one)", async ({
    page,
    mcp,
  }) => {
    await toggleAiAccess(page);
    await addCustomConnector(page, mcp);

    // Turning the master toggle off clears the custom URL by design.
    await toggleAiAccess(page);
    await expect(page.getByText("Trusted connector")).toBeHidden();
    await expect(page.getByText("mcp.id.ai", { exact: true })).toBeHidden();
    await expect(page.getByText("Internet Computer MCP")).toBeHidden();

    await toggleAiAccess(page);

    await expect(page.getByText("Internet Computer MCP")).toBeVisible();
    await expect(page.getByText("Official · Hosted by DFINITY")).toBeVisible();
    await expect(page.getByRole("button", { name: "Customize" })).toBeVisible();
    await expect(page.getByText("mcp.id.ai", { exact: true })).toBeHidden();
  });
});
