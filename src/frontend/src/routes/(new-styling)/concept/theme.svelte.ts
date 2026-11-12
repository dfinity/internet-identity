/**
 * A light/dark switch for trying the concept in both themes.
 *
 * The product follows the system theme through `prefers-color-scheme` media
 * queries (app.css, Tailwind's `dark:`, surfaces.css), which a page can't set.
 * So the switch rewrites those queries where they stand: forcing a theme makes
 * the queries for it always match and the others never; following the system
 * again puts every query back. Only this route does it, and leaving the route
 * undoes it. Images that carry their own query (some app logos) still follow
 * the system.
 */
import { MediaQuery } from "svelte/reactivity";

export type Theme = "light" | "dark";

const KEY = "ii-concept-theme";
const SCHEME = /\(\s*prefers-color-scheme\s*:\s*(light|dark)\s*\)/g;
const HAS_SCHEME = /prefers-color-scheme/;
// Stand-ins that every browser reads as always true and as never true.
const ALWAYS = "(min-width: 0px)";
const NEVER = "(max-width: 0px)";

/** Each query as the stylesheet wrote it, before any rewrite. */
const original = new WeakMap<MediaList, string>();

const visit = (rules: CSSRuleList, forced: Theme | undefined): void => {
  for (const rule of Array.from(rules)) {
    if (rule instanceof CSSMediaRule) {
      const written = original.get(rule.media) ?? rule.media.mediaText;
      if (HAS_SCHEME.test(written)) {
        original.set(rule.media, written);
        const next =
          forced === undefined
            ? written
            : written.replace(SCHEME, (_, scheme: string) =>
                scheme === forced ? ALWAYS : NEVER,
              );
        if (rule.media.mediaText !== next) {
          rule.media.mediaText = next;
        }
      }
    }
    // Media queries also sit inside layers, other media rules and, with CSS
    // nesting, style rules.
    if ("cssRules" in rule) {
      visit((rule as CSSGroupingRule).cssRules, forced);
    }
  }
};

const apply = (forced: Theme | undefined): void => {
  for (const sheet of Array.from(document.styleSheets)) {
    let rules: CSSRuleList;
    try {
      rules = sheet.cssRules;
    } catch {
      // A stylesheet from another origin can't be read, nor rewritten.
      continue;
    }
    visit(rules, forced);
  }
  // Form controls and scrollbars follow `color-scheme`, not the queries.
  document.documentElement.style.colorScheme = forced ?? "";
};

const read = (): Theme | undefined => {
  const stored = localStorage.getItem(KEY);
  return stored === "light" || stored === "dark" ? stored : undefined;
};

/** Create while a component initializes: it applies the theme for as long as
 *  that component lives. */
export class ConceptTheme {
  #system = new MediaQuery("(prefers-color-scheme: dark)");
  /** Set only while it differs from the system's theme. */
  #forced = $state<Theme | undefined>(read());

  constructor() {
    $effect(() => {
      const forced = this.#forced;
      apply(forced);
      // Styles arrive later too (a component's own, a hot reload), so new
      // ones get the same treatment.
      const observer = new MutationObserver(() => apply(forced));
      observer.observe(document.head, {
        childList: true,
        subtree: true,
        characterData: true,
      });
      return () => {
        observer.disconnect();
        apply(undefined);
      };
    });
  }

  get current(): Theme {
    return this.#forced ?? (this.#system.current ? "dark" : "light");
  }

  toggle = (): void => {
    const next: Theme = this.current === "dark" ? "light" : "dark";
    const system: Theme = this.#system.current ? "dark" : "light";
    this.#forced = next === system ? undefined : next;
    if (this.#forced === undefined) {
      localStorage.removeItem(KEY);
    } else {
      localStorage.setItem(KEY, this.#forced);
    }
  };
}
