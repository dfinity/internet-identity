import {
  BookOpenIcon,
  KeyRoundIcon,
  MailIcon,
  MonitorSmartphoneIcon,
  PlusIcon,
  UserRoundIcon,
} from "@lucide/svelte";
import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
import {
  brandIconOf,
  nameOf,
} from "../../../manage/(authenticated)/devices/browsers";
import type { Glyph } from "./Glyphs.svelte";
import type { MockBrowser } from "../../mock/mockData";
import {
  emailStatus,
  isOn,
  phraseStatus,
  waysLeft,
  type CardStatus,
  type MethodStatus,
  type Scenario,
} from "./status";

/** A way back in's tooltip, as its row in the section says where it
 *  stands. */
const RECOVERY_NOTE: Record<MethodStatus, string> = {
  off: "Not set up",
  unchecked: "Not verified yet",
  pending: "Waiting for the code",
  on: "Set up",
  inUse: "You're signed in with it now",
  new: "Reset 2 hours ago",
};

/**
 * What a card's message is about, as icons beside it: neutral, what's inside
 * (the ways in, the ways back in, the browsers); otherwise only the item it's
 * about, marked, as Thomas asked ("New sign-in" with the browser's icon, "3
 * browsers" with theirs). What isn't there yet is dashed. Each says on hover
 * what it is and where it stands, in the words its row uses in the section.
 */
export const glyphsOf = (
  card: CardStatus,
  scenario: Scenario,
  browsers: MockBrowser[],
  /** What this browser signed in with: the first way in. */
  signedInWith: string,
  /** Ways in removed from the Access section. */
  removedAccess: ReadonlySet<string> = new Set(),
): Glyph[] => {
  const neutral = card.tone === "neutral";
  if (card.kind === "access") {
    const yubikey: Glyph = {
      id: "yubikey",
      icon: KeyRoundIcon,
      label: "YubiKey",
      note:
        scenario.access === "unused"
          ? "Last used 8 months ago"
          : "Security key",
    };
    if (card.tone === "warning") {
      return [
        {
          id: "add",
          icon: PlusIcon,
          empty: true,
          label: "Another way to sign in",
          note: "Not added yet",
        },
      ];
    }
    if (!neutral) {
      return [{ ...yubikey, marked: true }];
    }
    const local = scenario.access === "oneLocal";
    const current: Glyph = {
      id: "current",
      icon: PasskeyIcon,
      label: local ? "Windows Hello" : signedInWith,
      note: local ? "Passkey on this device only" : "In use now",
    };
    const google: Glyph = {
      id: "google",
      icon: UserRoundIcon,
      label: "Google",
      note: "t•••@gmail.com",
    };
    const all = { current, yubikey, google } as Record<string, Glyph>;
    return waysLeft(scenario.access, removedAccess).map((id) => all[id]);
  }
  if (card.kind === "recovery") {
    const phrase = phraseStatus(scenario.recovery);
    const email = emailStatus(scenario.recovery);
    const phraseGlyph: Glyph = {
      id: "phrase",
      icon: BookOpenIcon,
      label: "Recovery phrase",
      note: RECOVERY_NOTE[phrase],
    };
    const emailGlyph: Glyph = {
      id: "email",
      icon: MailIcon,
      label: "Recovery email",
      note: RECOVERY_NOTE[email],
    };
    // A message about one way back in carries that way alone, marked.
    const about =
      email === "pending" || email === "inUse"
        ? emailGlyph
        : isOn(phrase) || phrase === "unchecked"
          ? phraseGlyph
          : undefined;
    if (!neutral && about !== undefined) {
      return [{ ...about, marked: true }];
    }
    if (!isOn(phrase) && !isOn(email)) {
      return [
        { ...phraseGlyph, empty: true },
        { ...emailGlyph, empty: true },
      ];
    }
    return [
      ...(isOn(phrase) ? [phraseGlyph] : []),
      ...(isOn(email) ? [emailGlyph] : []),
    ];
  }
  const shown = neutral
    ? browsers.slice(0, 3)
    : browsers.filter(({ isNew }) => isNew === true);
  return shown.map((browser) => ({
    id: browser.id,
    src: brandIconOf(browser.description),
    icon: MonitorSmartphoneIcon,
    marked: !neutral,
    label: nameOf(browser.description),
    note:
      browser.isCurrent === true
        ? "This browser"
        : browser.isNew === true
          ? "New sign-in"
          : "Signed in",
  }));
};
