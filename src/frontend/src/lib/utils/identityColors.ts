/**
 * Colours and initials for an identity's avatar, so that the identities on a
 * device tell apart at a glance.
 *
 * The pairs come from mymind's curated palettes
 * (https://access.mymind.com/colors): `background` is a palette's primary
 * colour and `foreground` the colour mymind sets on it, its accent — or, where
 * the accent is too faint for two letters, the palette's darkest companion.
 * Every pair keeps a contrast of at least 3.5:1. Most are light grounds with
 * dark letters, the rest saturated grounds with light ones, spread around the
 * hue wheel. Near-black grounds are left out: at 32px they all read as the
 * same dark disc, whatever their hue.
 */

export type IdentityPalette = {
  /** mymind's name for the palette. */
  name: string;
  background: string;
  foreground: string;
};

export const IDENTITY_PALETTES: readonly IdentityPalette[] = [
  // Light grounds.
  { name: "Packed Lunch", background: "#F1C3B8", foreground: "#19227D" },
  { name: "Penny Candy", background: "#EEC0DB", foreground: "#162BF4" },
  { name: "Block Party", background: "#75FFC0", foreground: "#7A30CF" },
  { name: "Solar Flare", background: "#F8DA1B", foreground: "#9A008A" },
  { name: "Golden Hour", background: "#FDB90B", foreground: "#321913" },
  { name: "No Dice", background: "#77EEDF", foreground: "#28292B" },
  { name: "Hidden Garden", background: "#9AB79A", foreground: "#094436" },
  { name: "Robed Witness", background: "#8AA9C6", foreground: "#093929" },
  { name: "Old Diary", background: "#C0C2CE", foreground: "#012F63" },
  { name: "Bite Your Tongue", background: "#E88565", foreground: "#181A27" },
  { name: "Diamond Day", background: "#FF6090", foreground: "#81072C" },
  { name: "Fool's Errand", background: "#FB9B2A", foreground: "#0C00FF" },
  { name: "Brain Waves", background: "#A6CFE2", foreground: "#0B0C11" },
  // Saturated grounds.
  { name: "Milo's Big Day", background: "#3333FF", foreground: "#CDFFCC" },
  { name: "Open Sea", background: "#083EA7", foreground: "#1FDE91" },
  { name: "Late Gift", background: "#9357A9", foreground: "#FFFFFF" },
  { name: "Pleasant Journey", background: "#E33946", foreground: "#FDEDB2" },
  { name: "Mr. Business", background: "#167C80", foreground: "#FFFFFF" },
  { name: "First Edition", background: "#0C485E", foreground: "#FEB904" },
  { name: "The Cathedral", background: "#424E22", foreground: "#FFAD67" },
  { name: "Rosebud", background: "#501718", foreground: "#FD750A" },
];

// FNV-1a, then murmur3's finaliser, so that consecutive identity numbers —
// the usual case for identities made one after another — land far apart.
const hash = (text: string): number => {
  let h = 0x811c9dc5;
  for (let i = 0; i < text.length; i++) {
    h ^= text.charCodeAt(i);
    h = Math.imul(h, 0x01000193);
  }
  h ^= h >>> 16;
  h = Math.imul(h, 0x85ebca6b);
  h ^= h >>> 13;
  h = Math.imul(h, 0xc2b2ae35);
  h ^= h >>> 16;
  return h >>> 0;
};

/**
 * The identity number, not the name, picks the colours: the name can be
 * changed and the credential differs per device, while the number is the same
 * everywhere the identity is used.
 */
export const identityPalette = (identityNumber: bigint): IdentityPalette =>
  IDENTITY_PALETTES[hash(identityNumber.toString()) % IDENTITY_PALETTES.length];

const leadingLetter = /^\p{L}\p{M}*/u;

/**
 * Initials the way DiceBear's initials style takes them: the first letters of
 * the first and last words, or the first two letters of a single word, so
 * "Thomas" and "Testing" stay "TH" and "TE" rather than both "T".
 * `undefined` when the name has no letters to take.
 */
export const identityInitials = (name?: string): string | undefined => {
  const words = name?.replace(/@.*/u, "").match(/\p{L}[\p{L}\p{M}]*/gu);
  if (words === null || words === undefined) {
    return undefined;
  }
  const initials =
    words.length === 1
      ? (words[0].match(/^(?:\p{L}\p{M}*){1,2}/u)?.[0] ?? "")
      : (words[0].match(leadingLetter)?.[0] ?? "") +
        (words[words.length - 1].match(leadingLetter)?.[0] ?? "");
  return initials.toLocaleUpperCase();
};
