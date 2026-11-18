import {
  BookOpenIcon,
  KeyRoundIcon,
  MailIcon,
  MonitorSmartphoneIcon,
  PlusIcon,
  UserRoundIcon,
} from "@lucide/svelte";
import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
import { brandIconOf } from "../manage/(authenticated)/devices/browsers";
import type { Glyph } from "./components/Glyphs.svelte";
import type { MockBrowser } from "./mockData";
import {
  accessCount,
  emailStatus,
  isOn,
  phraseStatus,
  type CardStatus,
  type Scenario,
} from "./status";

/**
 * What a card's message is about, as icons beside it: neutral, what's inside
 * (the ways in, the ways back in, the browsers); otherwise only the item it's
 * about, marked, as Thomas asked ("New sign-in" with the browser's icon, "3
 * browsers" with theirs). What isn't there yet is dashed.
 */
export const glyphsOf = (
  card: CardStatus,
  scenario: Scenario,
  browsers: MockBrowser[],
): Glyph[] => {
  const neutral = card.tone === "neutral";
  if (card.kind === "access") {
    if (card.tone === "warning") {
      return [{ id: "add", icon: PlusIcon, empty: true }];
    }
    if (!neutral) {
      return [{ id: "yubikey", icon: KeyRoundIcon, marked: true }];
    }
    return accessCount(scenario.access) === 1
      ? [{ id: "current", icon: PasskeyIcon }]
      : [
          { id: "current", icon: PasskeyIcon },
          { id: "yubikey", icon: KeyRoundIcon },
          { id: "google", icon: UserRoundIcon },
        ];
  }
  if (card.kind === "recovery") {
    const phrase = phraseStatus(scenario.recovery);
    const email = emailStatus(scenario.recovery);
    if (!isOn(phrase) && !isOn(email)) {
      return [
        { id: "phrase", icon: BookOpenIcon, empty: true },
        { id: "email", icon: MailIcon, empty: true },
      ];
    }
    if (!neutral) {
      return [{ id: "phrase", icon: BookOpenIcon, marked: true }];
    }
    return [
      ...(isOn(phrase) ? [{ id: "phrase", icon: BookOpenIcon }] : []),
      ...(isOn(email) ? [{ id: "email", icon: MailIcon }] : []),
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
  }));
};
