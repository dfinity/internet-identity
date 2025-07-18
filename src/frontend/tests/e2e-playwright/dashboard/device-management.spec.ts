import { expect, test } from "@playwright/test";
import {
  clearStorage,
  createIdentity,
  dummyAuth,
  II_URL,
  visibleAndClick,
} from "../utils";

const DEFAULT_USER_NAME = "Zhuangzi";
const ANOTHER_USER_NAME = "Laozi";

test("Add passkey on same device", async ({ page }) => {
  const auth = dummyAuth();
  const anotherAuth = dummyAuth();
  await createIdentity(page, DEFAULT_USER_NAME, auth);
  await page.goto(II_URL);
  auth(page);
  await visibleAndClick(page, "button", {
    name: DEFAULT_USER_NAME,
  });
  await page.waitForURL(II_URL + "/manage");
  await visibleAndClick(page, "link", {
    name: "Security",
    exact: true,
  });

  await page.waitForURL(II_URL + "/manage/security");
  await visibleAndClick(page, "button", {
    name: "Add",
  });
  await visibleAndClick(page, "button", {
    name: "Continue with Passkey",
  });
  anotherAuth(page);
  await visibleAndClick(page, "button", {
    name: "Create Passkey",
  });

  // Verify exactly two Chrome divs exist
  const chromeDivs = page.getByText("Chrome");
  await expect(chromeDivs).toHaveCount(2);

  // Verify the first Chrome div's parent container has the "Last used today at" text
  const firstChromeDiv = chromeDivs.first();
  const firstChromeContainer = firstChromeDiv.locator("..");
  await expect(
    firstChromeContainer.getByText(/Last used today at/),
  ).toBeVisible();

  // Optionally, verify the second Chrome div doesn't have this text (if that's expected)
  const secondChromeDiv = chromeDivs.nth(1);
  const secondChromeContainer = secondChromeDiv.locator("..");
  await expect(
    secondChromeContainer.getByText(/Last used today at/),
  ).not.toBeVisible();
});

test("Remove passkey after adding it", async ({ page }) => {
  const auth = dummyAuth();
  const anotherAuth = dummyAuth();
  await createIdentity(page, ANOTHER_USER_NAME, auth);
  await page.goto(II_URL);
  auth(page);
  await visibleAndClick(page, "button", {
    name: ANOTHER_USER_NAME,
  });
  await page.waitForURL(II_URL + "/manage");
  await visibleAndClick(page, "link", {
    name: "Security",
    exact: true,
  });

  await page.waitForURL(II_URL + "/manage/security");
  await visibleAndClick(page, "button", {
    name: "Add",
  });
  await visibleAndClick(page, "button", {
    name: "Continue with Passkey",
  });
  anotherAuth(page);
  await visibleAndClick(page, "button", {
    name: "Create Passkey",
  });

  // Verify exactly two Chrome divs exist
  const chromeDivs = page.getByText("Chrome");
  await expect(chromeDivs).toHaveCount(2);

  // Verify the first Chrome div's parent container has the "Last used today at" text
  const firstChromeDiv = chromeDivs.first();
  const firstChromeContainer = firstChromeDiv.locator("..");
  await expect(
    firstChromeContainer.getByText(/Last used today at/),
  ).toBeVisible();

  // Find the second Chrome div and locate the button that contains the trash icon
  const secondChromeDiv = chromeDivs.nth(1);
  const secondChromeContainer = secondChromeDiv.locator(".."); // Get parent of Chrome div

  // Find the button that contains the trash icon
  const removeButton = secondChromeContainer
    .locator("div")
    .locator("button")
    .filter({
      has: page.locator('[class*="lucide-trash-2"]'),
    })
    .first();
  await expect(removeButton).toBeVisible();

  // Click the remove button
  await removeButton.click();

  // Wait for the removal to complete
  await page.waitForTimeout(1000);

  // Verify that only one Chrome div remains
  await expect(chromeDivs).toHaveCount(1);
});
