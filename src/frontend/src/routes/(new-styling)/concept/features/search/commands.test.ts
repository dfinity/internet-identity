import { PlusIcon } from "@lucide/svelte";
import { describe, expect, it } from "vitest";
import { matchesCommand, type Command } from "./commands";

const command = (label: string, keywords?: string[]): Command => ({
  id: label,
  label,
  keywords,
  icon: PlusIcon,
});

describe("matchesCommand", () => {
  const recovery = command("Set up recovery", ["phrase", "email", "lost"]);
  const signOut = command("Sign out", ["logout"]);

  it("finds a command by the start of any of its words", () => {
    expect(matchesCommand(recovery, "rec")).toBe(true);
    expect(matchesCommand(recovery, "set")).toBe(true);
    expect(matchesCommand(recovery, "Up REC")).toBe(true);
  });

  it("finds it by a keyword the label doesn't say", () => {
    expect(matchesCommand(recovery, "phrase")).toBe(true);
    expect(matchesCommand(signOut, "logo")).toBe(true);
  });

  it("doesn't match letters inside a word", () => {
    expect(matchesCommand(recovery, "cover")).toBe(false);
    expect(matchesCommand(signOut, "ign")).toBe(false);
  });

  it("needs every word typed", () => {
    expect(matchesCommand(recovery, "set phrase")).toBe(true);
    expect(matchesCommand(recovery, "set apps")).toBe(false);
  });

  it("keeps a domain's dots in one word", () => {
    const start = command("Make id.ai your start page");
    expect(matchesCommand(start, "id.ai")).toBe(true);
    expect(matchesCommand(start, "start")).toBe(true);
  });
});
