/**
 * What the command line can do besides finding an app: the dashboard's own
 * steps, each a row to run. The page says which there are (it owns what they
 * do); the search lists the suggested ones before a letter is typed and
 * finds the rest by their words.
 */
import type { Component } from "svelte";
import type { IdentityPalette } from "$lib/utils/identityColors";
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";

export interface Command {
  id: string;
  /** What it does, as the row says it. */
  label: string;
  /** Where it leads or what it is about, at the row's end. */
  hint?: string;
  /** A glyph, or the identity it switches to. */
  icon:
    | Component
    | { identity: LastUsedIdentity; palette: IdentityPalette | undefined };
  /** Other words it is found by. */
  keywords?: string[];
  /** Offered before a letter is typed. */
  suggested?: boolean;
  /** The account waits on it: a dot on its glyph, as on the section's. */
  dot?: "risk" | "action" | "news";
  /** `apps`: stays in the search and turns it to adding an app. */
  opens?: "apps";
  run?: () => void;
}

/** Every word typed starts a word of the command's label or keywords, so
 *  "rec" finds recovery and "o" doesn't find everything with an o in it. */
export const matchesCommand = (command: Command, typed: string): boolean => {
  const words = [command.label, ...(command.keywords ?? [])]
    .join(" ")
    .toLowerCase()
    .split(/[^\p{L}\p{N}.]+/u)
    .filter((word) => word !== "");
  return typed
    .toLowerCase()
    .split(/\s+/)
    .filter((word) => word !== "")
    .every((part) => words.some((word) => word.startsWith(part)));
};
