import type { Component } from "svelte";

/** One thing a card's message is about: a glyph, or a browser's logo. */
export interface Glyph {
  id: string;
  icon?: Component;
  src?: string;
  /** What it is, said on hover: "Recovery phrase", "Firefox on Windows". */
  label: string;
  /** Where it stands, under the label: "Not set up", "New sign-in". */
  note?: string;
  /** Not there yet (a way back in not set up, a way in to add): a dashed
   *  outline, as an empty row in the section has it. */
  empty?: boolean;
  /** The item the message is about: it carries the style's dot, the same
   *  cue its row carries once the card is open. */
  marked?: boolean;
}
