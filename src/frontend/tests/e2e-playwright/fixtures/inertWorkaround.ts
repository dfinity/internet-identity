import { test as base } from "@playwright/test";

/**
 * Playwright does not exclude `inert` elements from its accessibility
 * tree, causing role-based locators to match elements that should be
 * hidden. Svelte's transition system relies heavily on `inert` while
 * elements are transitioning in or out, which leads to flaky strict
 * mode violations without this workaround.
 *
 * This global fixture injects a MutationObserver that syncs the
 * `inert` attribute to `aria-hidden`, which Playwright does respect.
 *
 * https://github.com/microsoft/playwright/issues/36938
 */
export const test = base.extend({
  page: async ({ page }, use) => {
    await page.addInitScript(() => {
      const sync = (el: Element) => {
        if (el.hasAttribute("inert")) {
          el.setAttribute("aria-hidden", "true");
        } else {
          el.removeAttribute("aria-hidden");
        }
      };
      new MutationObserver((mutations) => {
        for (const m of mutations) {
          if (m.type === "attributes" && m.target instanceof Element) {
            sync(m.target);
          }
        }
      }).observe(document, {
        attributes: true,
        attributeFilter: ["inert"],
        subtree: true,
      });
    });
    await use(page);
  },
});
