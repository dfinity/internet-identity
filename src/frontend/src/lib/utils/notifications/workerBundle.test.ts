import { describe, expect, it } from "vitest";
import { existsSync, readFileSync } from "node:fs";

/**
 * The service worker is a module worker, so everything in its import graph is evaluated
 * where there is no `document`. One import of `$lib/globals` was enough to pull lit-html
 * in through the page's error template, and lit-html reads `document` at module scope:
 * the worker then failed to evaluate, `register()` rejected, and the install reported
 * "Can't find variable: document" with nothing pointing at the cause.
 *
 * Skipped where the build output is absent, since `npm run check` runs without one.
 */
const BUNDLE = ".svelte-kit/output/client/service-worker.js";

describe.runIf(existsSync(BUNDLE))("the built service worker", () => {
  it("pulls in nothing that reads the DOM while it loads", () => {
    const source = readFileSync(BUNDLE, "utf8");

    // lit-html's module scope does `const l = document, P = l.createTreeWalker(l, 129)`.
    expect(source).not.toContain("createTreeWalker");
  });
});
