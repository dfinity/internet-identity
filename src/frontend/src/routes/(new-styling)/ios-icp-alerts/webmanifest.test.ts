// The path constant's module opens the browser key database as it is imported, and jsdom serves no IndexedDB.
import "fake-indexeddb/auto";
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { NOTIFICATION_APP_PATH } from "$lib/utils/notifications/appPath";
import { NOTIFICATION_APP_NAME } from "./appName";

/** Read off disk, because the manifest is served as a static file and nothing in the bundle imports it. */
const manifest = JSON.parse(
  readFileSync(
    resolve(
      dirname(fileURLToPath(import.meta.url)),
      "../../../../static/ios-icp-alerts.webmanifest",
    ),
    "utf-8",
  ),
) as Record<string, string>;

describe("the Home Screen app's manifest", () => {
  it.each(["name", "short_name"])("names the app in %s", (field) => {
    expect(manifest[field]).toBe(NOTIFICATION_APP_NAME);
  });

  it.each(["id", "start_url", "scope"])("serves the app at %s", (field) => {
    expect(manifest[field]).toBe(NOTIFICATION_APP_PATH);
  });
});
