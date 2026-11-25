/**
 * The landing playground's state: which layout, which identities, which
 * texture, and what the visitor is doing (hovering, signing in, editing).
 * Every choice is in the address too, so a link opens the same combination,
 * and the frames of the phone and the overview read theirs from it.
 */
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { identityPalette } from "$lib/utils/identityColors";
import { toaster } from "$lib/components/utils/toaster";
import { IDENTITIES_BY_STATE, nameOf, type StateId } from "./mock";

export type { StateId };

export type LayoutId =
  "centered" | "split" | "profiles" | "welcome" | "sentence" | "immersive";

export type TextureId =
  | "none"
  | "flair"
  | "magnet"
  | "watch"
  | "twist"
  | "blinds"
  | "cells"
  | "swell";

export type BrandId = "statement" | "quote" | "principles";
export type ToneId = "neutral" | "identity";
export type ViewId = "page" | "phone" | "all";

export interface LayoutOption {
  id: LayoutId;
  label: string;
  group: string;
  /** What to look at in it, one line, for the menu and the overview. */
  note: string;
  /** Its footer holds the flask that opens the controls; elsewhere the
   *  flask floats in the middle of the header. */
  labInFooter?: boolean;
}

export const LAYOUTS: LayoutOption[] = [
  {
    id: "profiles",
    label: "Profiles",
    group: "Priority",
    labInFooter: true,
    note: "Identities as large tiles, like the apps on My apps. The colour tells them apart before the name does.",
  },
  {
    id: "centered",
    label: "Centered",
    group: "Conventional",
    note: "One card in the middle, the way most sign-in pages do it. Nothing to learn.",
  },
  {
    id: "split",
    label: "Split",
    group: "Conventional",
    note: "Choosing on the left, the brand on the right: room to say what Internet Identity stands for.",
  },
  {
    id: "welcome",
    label: "Welcome back",
    group: "Alternative",
    note: "The last identity used is the page; the others wait in a stack, as on the dashboard's identity card.",
  },
  {
    id: "sentence",
    label: "Sentence",
    group: "Alternative",
    note: "The choice is a sentence, the identities its words. Editorial, and it only works while the list is short.",
  },
  {
    id: "immersive",
    label: "Immersive",
    group: "Alternative",
    note: "The texture is the page; the identities float in a dock at the bottom, where a thumb reaches.",
  },
];

export const STATES: { id: StateId; label: string; note: string }[] = [
  { id: "none", label: "None", note: "No identity yet: sign in or create" },
  { id: "one", label: "One", note: "A single identity" },
  { id: "two", label: "Two", note: "Two identities, a passkey and SSO" },
  { id: "several", label: "Three", note: "Three identities on this device" },
  { id: "many", label: "Many", note: "Seven, to see where a layout runs out" },
];

export const TEXTURES: { id: TextureId; label: string; note: string }[] = [
  { id: "none", label: "None", note: "Type and surfaces only" },
  {
    id: "flair",
    label: "Dots",
    note: "The product's own FlairCanvas, as on today's landing page",
  },
  {
    id: "magnet",
    label: "Magnet",
    note: "Dots near the pointer gather into a block",
  },
  { id: "watch", label: "Watch", note: "A field of eyes that follow you" },
  { id: "twist", label: "Twist", note: "Every bar turns to the pointer" },
  { id: "blinds", label: "Blinds", note: "Stripes open like ribbons turning" },
  { id: "cells", label: "Cells", note: "Squares swell into a soft dome" },
  { id: "swell", label: "Swell", note: "Dashes thicken and part" },
];

export const BRANDS: { id: BrandId; label: string }[] = [
  { id: "statement", label: "Statement" },
  { id: "quote", label: "Quote" },
  { id: "principles", label: "Principles" },
];

export const TONES: { id: ToneId; label: string; note: string }[] = [
  { id: "neutral", label: "Neutral", note: "On the concept's surfaces" },
  {
    id: "identity",
    label: "Identity colour",
    note: "In the colours of the identity you point at",
  },
];

/** Where each layout starts, so switching to it shows it at its best. */
export const DEFAULT_TEXTURE: Record<LayoutId, TextureId> = {
  centered: "none",
  split: "blinds",
  profiles: "none",
  welcome: "cells",
  sentence: "none",
  immersive: "magnet",
};

const pick = <T extends string>(
  value: string | null,
  options: readonly { id: T }[],
  fallback: T,
): T => options.find((one) => one.id === value)?.id ?? fallback;

export class Playground {
  layout = $state<LayoutId>("profiles");
  state = $state<StateId>("two");
  lastUsed = $state(true);
  texture = $state<TextureId>("none");
  brand = $state<BrandId>("statement");
  tone = $state<ToneId>("neutral");
  view = $state<ViewId>("page");
  /** Inside the phone's or the overview's frame: no controls. */
  framed = $state(false);
  /** Whether the controls are open. They start folded into the flask, so
   *  the page is seen as it would be. */
  controls = $state(false);

  /** Identities removed on this visit, by number. */
  removed = $state<string[]>([]);
  /** Whether the list shows its remove buttons. */
  editing = $state(false);
  /** The identity signing in, for its spinner. */
  pending = $state<bigint>();
  /** The identity pointed at or focused, which the brand side takes the
   *  colour of. */
  hovered = $state<bigint>();
  /** Whether the ways to sign in are open over the page. */
  methodsOpen = $state(false);

  identities = $derived(
    IDENTITIES_BY_STATE[this.state].filter(
      (one) => !this.removed.includes(`${one.identityNumber}`),
    ),
  );
  /** The identity used most recently, which the "Last used" mark is on. */
  recent = $derived<LastUsedIdentity | undefined>(this.identities[0]);
  /** The identity the brand side speaks to: the one pointed at, else the
   *  most recent. */
  focus = $derived<LastUsedIdentity | undefined>(
    this.identities.find((one) => one.identityNumber === this.hovered) ??
      this.recent,
  );
  focusPalette = $derived(
    this.focus === undefined
      ? undefined
      : identityPalette(this.focus.identityNumber),
  );

  /** Read the address, once, as the page opens. */
  load = (params: URLSearchParams): void => {
    this.layout = pick(params.get("layout"), LAYOUTS, this.layout);
    this.state = pick(params.get("state"), STATES, this.state);
    this.texture = pick(
      params.get("texture"),
      TEXTURES,
      DEFAULT_TEXTURE[this.layout],
    );
    this.brand = pick(params.get("brand"), BRANDS, this.brand);
    this.tone = pick(params.get("tone"), TONES, this.tone);
    this.view = pick(
      params.get("view"),
      [{ id: "page" }, { id: "phone" }, { id: "all" }] as const,
      this.view,
    );
    this.lastUsed = params.get("lastUsed") !== "0";
    this.framed = params.get("frame") === "1";
  };

  /** The address for this combination; `extra` for a frame's own. */
  query = (extra: Record<string, string> = {}): string => {
    const params: Record<string, string> = {
      layout: this.layout,
      state: this.state,
      texture: this.texture,
      brand: this.brand,
      tone: this.tone,
      ...(this.lastUsed ? {} : { lastUsed: "0" }),
      ...(this.view === "page" || this.framed ? {} : { view: this.view }),
      ...extra,
    };
    return `?${Object.entries(params)
      .map(([key, value]) => `${key}=${encodeURIComponent(value)}`)
      .join("&")}`;
  };

  setLayout = (id: LayoutId): void => {
    if (id === this.layout) {
      return;
    }
    this.layout = id;
    this.texture = DEFAULT_TEXTURE[id];
    this.editing = false;
  };

  stepLayout = (by: number): void => {
    const index = LAYOUTS.findIndex((one) => one.id === this.layout);
    this.setLayout(LAYOUTS[(index + by + LAYOUTS.length) % LAYOUTS.length].id);
  };

  setState = (id: StateId): void => {
    this.state = id;
    this.removed = [];
    this.editing = false;
    this.hovered = undefined;
  };

  stepTexture = (): void => {
    const index = TEXTURES.findIndex((one) => one.id === this.texture);
    this.texture = TEXTURES[(index + 1) % TEXTURES.length].id;
  };

  /** Signing in, as far as a mock goes: a spinner, then the dashboard
   *  would open. */
  signIn = (one: LastUsedIdentity): void => {
    if (this.pending !== undefined || this.editing) {
      return;
    }
    this.pending = one.identityNumber;
    setTimeout(() => {
      this.pending = undefined;
      toaster.create({
        title: `Signed in as ${nameOf(one)}`,
        description: "Here the dashboard would open.",
        duration: 2500,
      });
    }, 1100);
  };

  /** A way to sign in or to create an identity, from the empty state or
   *  Add identity. */
  useMethod = (label: string): void => {
    this.methodsOpen = false;
    toaster.create({
      title: label,
      description: "Here the passkey prompt or the provider's window opens.",
      duration: 2500,
    });
  };

  remove = (one: LastUsedIdentity): void => {
    this.removed = [...this.removed, `${one.identityNumber}`];
    if (this.identities.length === 0) {
      this.editing = false;
    }
    toaster.create({
      title: "Identity removed",
      description: `${nameOf(one)} has been removed from this device.`,
      closable: true,
      duration: 4000,
      action: {
        label: "Undo",
        onClick: () => {
          this.removed = this.removed.filter(
            (id) => id !== `${one.identityNumber}`,
          );
        },
      },
    });
  };
}
