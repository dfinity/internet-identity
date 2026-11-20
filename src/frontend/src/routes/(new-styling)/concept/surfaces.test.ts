/**
 * The surface rule (SURFACES.md), checked: surfaces only go up. The ladder
 * itself only gets lighter step by step in dark, and no component of the
 * concept paints a background outside it, where in dark it would sit below
 * what it lies on and read as cut out of it.
 *
 * A failure names the line, what it breaks and what to use instead.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

// Read from disk: the test run leaves CSS imports empty.
const ladder = readFileSync(join(__dirname, "surfaces.css"), "utf8");

const components = import.meta.glob<string>("./**/*.svelte", {
  query: "?raw",
  import: "default",
  eager: true,
});

interface Rule {
  name: string;
  pattern: RegExp;
  why: string;
}

const INSTEAD =
  "Use an Elevated (a step up from where it lies), the step it is by name (--ii-canvas, --ii-floating, --ii-top), surfaceColor(useSurface() + 1), or an overlay (--ii-hover, --ii-selected). See SURFACES.md.";

const RULES: Rule[] = [
  {
    name: "a page background",
    pattern:
      /\bbg-bg-(?:primary|primary_alt|secondary|secondary_alt|tertiary|quaternary)\b/,
    why: "the product's page backgrounds sit below the ladder in dark (#161616 and #1a1a1a against the canvas's #252525), so whatever wears one on a card or the canvas reads as cut out of it",
  },
  {
    name: "a fixed colour",
    pattern:
      /\b(?:bg|fill)-(?:white|black|(?:neutral|gray|zinc|stone|slate)-\d+)\b/,
    why: "it stays as it is in dark, where every step of the ladder turns lighter",
  },
  {
    name: "a level by number",
    pattern: /(?:--|\b)ii-surface-\d/,
    why: "a number says nothing of where the surface stands, and copied elsewhere it lands under what it should sit on",
  },
];

/** Where the ladder lives, or what isn't the product: these may break it. */
const OUTSIDE = new Map<string, string>([
  ["./components/Elevated.svelte", "it is the ladder"],
  ["./components/SurfaceLevel.svelte", "it is the ladder"],
  [
    "./components/StateBoard.svelte",
    "the concept's review board of card states, not a screen of the product",
  ],
  [
    "./components/ExploreBuild.svelte",
    "Caffeine's banner: its own light sky in both themes ([color-scheme:light]), its white and black the brand's",
  ],
]);

/** Single lines that break a rule on purpose, each with why. */
const EXCEPTIONS: { file: string; line: string; why: string }[] = [
  {
    file: "./components/ExploreBanner.svelte",
    line: 'item.dark ? "bg-white text-neutral-950" : "bg-neutral-950 text-white"',
    why: "a brand banner's button, in the banner's own colours in both themes",
  },
  {
    file: "./components/SearchOverlay.svelte",
    line: "bg-black/5 [backdrop-filter",
    why: "the scrim under search: it dims and blurs the page, it isn't a surface",
  },
];

/** Breaks of the rule known and being fixed, each by its own commit. */
const PENDING: { file: string; line: string }[] = [
  {
    file: "./components/SectionView.svelte",
    line: 'info: "border-border-tertiary bg-bg-primary',
  },
  {
    file: "./components/SectionView.svelte",
    line: 'neutral: "border-border-tertiary bg-bg-primary',
  },
  {
    file: "./components/AppTile.svelte",
    line: ': "border-border-primary bg-bg-primary",',
  },
  {
    file: "./components/ManageHints.svelte",
    line: '[MARK, "border-border-primary bg-bg-primary"]',
  },
  {
    file: "./components/IdentityStack.svelte",
    line: 'class="bg-bg-secondary text-text-tertiary',
  },
];

const allowed = (file: string, text: string): boolean =>
  [...EXCEPTIONS, ...PENDING].some(
    (one) => one.file === file && text.includes(one.line),
  );

describe("the surface ladder", () => {
  /** Relative luminance of a #rrggbb colour. */
  const luminance = (hex: string): number => {
    const [r, g, b] = [1, 3, 5].map((at) => {
      const c = parseInt(hex.slice(at, at + 2), 16) / 255;
      return c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
    });
    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  };
  const levels = (css: string): number[] =>
    Array.from(css.matchAll(/--ii-surface-(\d):\s*(#[0-9a-f]{6})/gi))
      .sort(([, a], [, b]) => Number(a) - Number(b))
      .map(([, , hex]) => luminance(hex));
  const [light, dark] = ladder.split("@media (prefers-color-scheme: dark)");

  it("gets lighter with every step in dark", () => {
    const steps = levels(dark);
    expect(steps).toHaveLength(8);
    steps.slice(1).forEach((step, index) => {
      expect(step, `level ${index + 2} against ${index + 1}`).toBeGreaterThan(
        steps[index],
      );
    });
  });

  it("never gets darker in light, where shadow carries the height", () => {
    const steps = levels(light);
    expect(steps).toHaveLength(8);
    steps.slice(1).forEach((step, index) => {
      expect(
        step,
        `level ${index + 2} against ${index + 1}`,
      ).toBeGreaterThanOrEqual(steps[index]);
    });
  });
});

describe("the concept's components", () => {
  it("found the components", () => {
    expect(Object.keys(components).length).toBeGreaterThan(20);
  });

  it("paint no background outside the ladder", () => {
    const broken: string[] = [];
    for (const [file, source] of Object.entries(components)) {
      if (OUTSIDE.has(file)) {
        continue;
      }
      source.split("\n").forEach((text, index) => {
        for (const rule of RULES) {
          const found = text.match(rule.pattern);
          if (found !== null && !allowed(file, text)) {
            broken.push(
              `${file}:${index + 1}: "${found[0]}" is ${rule.name}: ${rule.why}.`,
            );
          }
        }
      });
    }
    expect(
      broken,
      `Surfaces only go up from what they lie on.\n${broken.join("\n")}\n${INSTEAD}`,
    ).toEqual([]);
  });

  it("keeps no exception that no longer matches", () => {
    const stale = [...EXCEPTIONS, ...PENDING].filter(
      ({ file, line }) => components[file]?.includes(line) !== true,
    );
    expect(stale).toEqual([]);
  });
});
