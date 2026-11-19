/**
 * The concept's few transitions, so every part moves the same way.
 *
 * Values follow Taste Machine's motion cards, with Emil Kowalski's position where
 * sources disagree: ease-out for anything that appears, exits quicker and quieter
 * than entrances, no bounce on a click, and under reduced motion only opacity.
 *
 * Exits ease out too. An ease-in exit holds the old thing at full strength for
 * the first half of its time, so it sits on top of what replaces it; easing
 * out, it is mostly gone in the first few frames.
 */
import { quintOut } from "svelte/easing";
import type { TransitionConfig } from "svelte/transition";

interface SwapParams {
  reduced?: boolean;
  delay?: number;
}

/** A glyph arriving in a shared cell: scale 0.25 → 1, blur 4px → 0, 300ms. */
export const swapIn = (
  _node: Element,
  { reduced = false, delay = 0 }: SwapParams = {},
): TransitionConfig => ({
  delay,
  duration: reduced ? 150 : 300,
  easing: quintOut,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: scale(${0.25 + 0.75 * t}); filter: blur(${(1 - t) * 4}px)`,
});

/** The glyph it replaces leaves faster, so an arrival outlasts a departure. */
export const swapOut = (
  _node: Element,
  { reduced = false }: SwapParams = {},
): TransitionConfig => ({
  duration: reduced ? 100 : 150,
  easing: quintOut,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: scale(${0.25 + 0.75 * t}); filter: blur(${(1 - t) * 4}px)`,
});

interface ShiftParams {
  /** +1 going deeper, −1 going back; already mirrored for RTL by the caller. */
  direction: number;
  reduced?: boolean;
}

/** A panel view arriving: a short shift from the side it comes from, 200ms. */
export const viewIn = (
  _node: Element,
  { direction, reduced = false }: ShiftParams,
): TransitionConfig => ({
  duration: reduced ? 150 : 200,
  easing: quintOut,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: translateX(${(1 - t) * 16 * direction}px)`,
});

/** The view it replaces just fades, quicker than the entrance. */
export const viewOut = (
  _node: Element,
  { reduced = false }: { reduced?: boolean } = {},
): TransitionConfig => ({
  duration: reduced ? 100 : 120,
  easing: quintOut,
  css: (t) => `opacity: ${t}`,
});

/** Island content arriving through a blur, after the shell starts to move. */
export const islandIn = (
  _node: Element,
  { reduced = false }: { reduced?: boolean } = {},
): TransitionConfig => ({
  delay: reduced ? 0 : 60,
  duration: reduced ? 150 : 260,
  easing: quintOut,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: scale(${0.96 + 0.04 * t}); filter: blur(${(1 - t) * 4}px)`,
});

/** Island content leaving: a plain quick fade, no blur. */
export const islandOut = (
  _node: Element,
  { reduced = false }: { reduced?: boolean } = {},
): TransitionConfig => ({
  duration: reduced ? 80 : 100,
  easing: quintOut,
  css: (t) => `opacity: ${t}`,
});

/** The island rising into place when it appears. */
export const shellIn = (
  _node: Element,
  { reduced = false }: { reduced?: boolean } = {},
): TransitionConfig => ({
  duration: reduced ? 150 : 280,
  easing: quintOut,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: translateY(${(1 - t) * 16}px)`,
});

/** And sinking away, shorter: a 12px drop over 150ms. */
export const shellOut = (
  _node: Element,
  { reduced = false }: { reduced?: boolean } = {},
): TransitionConfig => ({
  duration: reduced ? 100 : 150,
  easing: quintOut,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: translateY(${(1 - t) * 12}px)`,
});

/** CSS `cubic-bezier()` as an easing, so a Svelte transition can match a CSS one. */
const cubicBezier = (x1: number, y1: number, x2: number, y2: number) => {
  const axis = (p1: number, p2: number) => (s: number) =>
    3 * p1 * s * (1 - s) ** 2 + 3 * p2 * s ** 2 * (1 - s) + s ** 3;
  const x = axis(x1, x2);
  const y = axis(y1, y2);
  return (t: number): number => {
    // x(s) only grows while x1 and x2 stay in [0, 1], so halving finds it.
    let low = 0;
    let high = 1;
    for (let step = 0; step < 20; step++) {
      const mid = (low + high) / 2;
      if (x(mid) < t) {
        low = mid;
      } else {
        high = mid;
      }
    }
    return y((low + high) / 2);
  };
};

/** The product Popover's curve (`$lib/components/ui/Popover.svelte`). */
const popoverEase = cubicBezier(0.4, 0, 0.2, 1);

/** A menu opening the way the product's popovers do: scale 0.95 → 1 with
 *  opacity over 200ms, grown from the point it was opened at. */
export const menuIn = (
  _node: Element,
  { reduced = false }: { reduced?: boolean } = {},
): TransitionConfig => ({
  duration: reduced ? 150 : 200,
  easing: popoverEase,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: scale(${0.95 + 0.05 * t})`,
});

/** And closing like them too, quicker: back to 0.95 over 160ms. */
export const menuOut = (
  _node: Element,
  { reduced = false }: { reduced?: boolean } = {},
): TransitionConfig => ({
  duration: reduced ? 100 : 160,
  easing: popoverEase,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: scale(${0.95 + 0.05 * t})`,
});

interface WidgetParams {
  reduced?: boolean;
  /** Widgets arrive one after another, 50ms apart. */
  index?: number;
}

/** A widget rising into the dock through a blur, each a beat after the last. */
export const widgetIn = (
  _node: Element,
  { reduced = false, index = 0 }: WidgetParams = {},
): TransitionConfig => ({
  delay: reduced ? 0 : 40 + index * 50,
  duration: reduced ? 150 : 320,
  easing: quintOut,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: translateY(${(1 - t) * 12}px) scale(${0.96 + 0.04 * t}); filter: blur(${(1 - t) * 4}px)`,
});

/** A widget whose step is done: it shrinks a little and blurs away, quickly. */
export const widgetOut = (
  _node: Element,
  { reduced = false }: WidgetParams = {},
): TransitionConfig => ({
  duration: reduced ? 100 : 180,
  easing: quintOut,
  css: (t) =>
    reduced
      ? `opacity: ${t}`
      : `opacity: ${t}; transform: scale(${0.94 + 0.06 * t}); filter: blur(${(1 - t) * 4}px)`,
});

/** A thrown thing rising: exactly a parabola, slowing to the top. */
const RISE = "cubic-bezier(0.333, 0.667, 0.667, 1)";
/** And falling back down the same parabola, speeding up to the ground. */
const FALL = "cubic-bezier(0.333, 0, 0.667, 0.333)";

interface HopParams {
  /** How high the first hop goes, in px. */
  height?: number;
  /** How long the first hop stays in the air, in ms. */
  airtime?: number;
  /** The share of its speed a landing gives back: each hop after the first is
   *  that much shorter and restitution² as high. */
  restitution?: number;
  hops?: number;
}

/** Asking for the user the way a Dock icon does: a hop under gravity, then a
 *  smaller one as it lands. It moves `translate`, so it runs alongside any
 *  `transform` the element has, and it starts and ends at rest. */
export const hop = (
  node: Element,
  { height = 14, airtime = 440, restitution = 0.5, hops = 2 }: HopParams = {},
): Animation => {
  const frames: Keyframe[] = [{ translate: "0 0", easing: RISE }];
  const times = [0];
  let elapsed = 0;
  for (let index = 0; index < hops; index++) {
    const air = airtime * restitution ** index;
    const peak = height * restitution ** (2 * index);
    frames.push(
      { translate: `0 ${-peak}px`, easing: FALL },
      { translate: "0 0", easing: RISE },
    );
    times.push(elapsed + air / 2, elapsed + air);
    elapsed += air;
  }
  return node.animate(
    frames.map((frame, index) => ({
      ...frame,
      offset: times[index] / elapsed,
    })),
    { duration: elapsed },
  );
};

/** The concept's `--ii-ease-out`, for animations run from script. */
const EASE_OUT = "cubic-bezier(0.23, 1, 0.32, 1)";

/** A tab's page leaving: its content steps 8px away from the side the next
 *  page lies on and fades, 120ms. It holds there, out of sight, so the pager
 *  can move under it unseen. It leaves from where it stands, read off the
 *  screen: an arrival cut short leaves from where it got to (an implied first
 *  keyframe would start it back at full strength). */
export const pageLeave = (
  content: ArrayLike<Element>,
  { direction, reduced = false }: ShiftParams,
): Animation[] =>
  Array.from(content, (node) => {
    const { opacity, translate } = getComputedStyle(node);
    return node.animate(
      reduced
        ? [{ opacity }, { opacity: 0 }]
        : [
            { opacity, translate },
            { opacity: 0, translate: `${-8 * direction}px 0` },
          ],
      { duration: reduced ? 100 : 120, easing: EASE_OUT, fill: "forwards" },
    );
  });

/** The tab's own page arriving: 16px in from its side as it fades in, over
 *  240ms, a panel view's shift held a little longer as it fills the canvas. */
export const pageArrive = (
  content: ArrayLike<Element>,
  { direction, reduced = false }: ShiftParams,
): Animation[] =>
  Array.from(content, (node) =>
    node.animate(
      reduced
        ? [{ opacity: 0 }, { opacity: 1 }]
        : [
            { opacity: 0, translate: `${16 * direction}px 0` },
            { opacity: 1, translate: "0 0" },
          ],
      { duration: reduced ? 150 : 240, easing: EASE_OUT },
    ),
  );

/** Where an item of a list stood, against the list (its offset parent). */
export interface Spot {
  top: number;
  left: number;
  width: number;
}

/** Where each `[data-morph]` item of a list stands now, by its id. */
export const spotsOf = (list: HTMLElement): Record<string, Spot> =>
  Object.fromEntries(
    Array.from(list.querySelectorAll<HTMLElement>(":scope > [data-morph]")).map(
      (item) => [
        item.dataset.morph,
        { top: item.offsetTop, left: item.offsetLeft, width: item.offsetWidth },
      ],
    ),
  );

interface MorphParams {
  /** The list: its `[data-morph]` items, one of which grows. */
  list: HTMLElement;
  /** The item that grows or shrinks back. */
  id: string;
  opening: boolean;
  /** Changes the state and resolves once the DOM shows it. Before it, the
   *  page holds the others where they stood (`spotsOf`) as they fade; the
   *  card keeps both its layers, `[data-layer="summary"]` and
   *  `[data-layer="content"]`, the leaving one out of the flow. */
  change: () => Promise<void>;
  /** Called once, as the list is laid out the new way: on an opening, the
   *  panel goes back to its top, where the identity over the grown card
   *  is. */
  settle?: () => void;
  reduced: boolean;
}

/**
 * A card of a list growing into its whole section, and back: FLIP on the
 * card's box (`[data-morph-box]`, which clips what it holds). Measured, then
 * changed, then measured again, it starts where it stood and glides to where
 * it now is, its height following, while the preview and the section cross
 * inside it and the other items fade where they stood.
 *
 * Opening takes 420ms, a large move that a strong ease-out still makes feel
 * quick; closing, 340ms. Under reduced motion only opacity moves. Resolves
 * when the card has landed, false if a newer morph took over.
 */
export const morph = async ({
  list,
  id,
  opening,
  change,
  settle,
  reduced,
}: MorphParams): Promise<boolean> => {
  const item = list.querySelector<HTMLElement>(`:scope > [data-morph="${id}"]`);
  const box = item?.querySelector<HTMLElement>("[data-morph-box]");
  if (item === null || box === null || box === undefined) {
    await change();
    return true;
  }
  const others = Array.from(
    list.querySelectorAll<HTMLElement>(":scope > [data-morph]"),
  ).filter((one) => one !== item);
  // Measured mid-flight too, so a reversal starts from where things are.
  const first = box.getBoundingClientRect();
  const shown = new Map(
    others.map((one) => [one, Number(getComputedStyle(one).opacity)]),
  );
  // Only what a morph runs: Svelte's own transitions inside stay theirs.
  for (const one of [
    box,
    ...others,
    ...Array.from(box.querySelectorAll<HTMLElement>("[data-layer]")),
  ]) {
    for (const animation of one.getAnimations()) {
      animation.cancel();
    }
  }
  for (const one of [item, ...others]) {
    one.style.height = "";
  }

  await change();
  settle?.();
  const last = box.getBoundingClientRect();
  const duration = opening ? 420 : 340;

  if (!opening) {
    // Its slot is already the small card's, so the items after it stand
    // where they'll be while it shrinks back down over them.
    item.style.height = `${last.height}px`;
  }

  const layers = {
    summary: box.querySelector<HTMLElement>('[data-layer="summary"]'),
    content: box.querySelector<HTMLElement>('[data-layer="content"]'),
  };
  const arriving = opening ? layers.content : layers.summary;
  const leaving = opening ? layers.summary : layers.content;
  arriving?.animate([{ opacity: 0 }, { opacity: 1 }], {
    delay: reduced ? 0 : 40,
    duration: reduced ? 150 : opening ? 280 : 220,
    easing: EASE_OUT,
    fill: "backwards",
  });
  leaving?.animate([{ opacity: 1 }, { opacity: 0 }], {
    duration: reduced ? 100 : 120,
    easing: EASE_OUT,
    fill: "forwards",
  });

  for (const one of others) {
    const from = shown.get(one) ?? 1;
    one.animate(
      opening
        ? [
            { opacity: from, transform: "scale(1)" },
            { opacity: 0, transform: reduced ? "none" : "scale(0.97)" },
          ]
        : [
            { opacity: from, transform: reduced ? "none" : "scale(0.97)" },
            { opacity: 1, transform: "none" },
          ],
      opening
        ? {
            duration: reduced ? 100 : 180,
            easing: EASE_OUT,
            fill: "forwards",
          }
        : {
            delay: reduced ? 0 : 80,
            duration: reduced ? 150 : 260,
            easing: EASE_OUT,
            fill: "backwards",
          },
    );
  }

  const landing = reduced
    ? undefined
    : box.animate(
        [
          {
            transform: `translateY(${first.top - last.top}px)`,
            height: `${first.height}px`,
          },
          { transform: "none", height: `${last.height}px` },
        ],
        { duration, easing: EASE_OUT },
      );
  try {
    await (landing?.finished ??
      new Promise((resolve) => setTimeout(resolve, 150)));
  } catch {
    // Cancelled: a newer morph took over and measured from here.
    return false;
  }
  item.style.height = "";
  return true;
};
