/** The copy and internationalization used in Internet Identity, based on
 * the officially supported languages */

import { I18n as BaseI18n } from "$lib/utils/i18n";

export type { DynamicKey } from "$lib/utils/i18n";

const _supportedLanguages = ["en"] as const;
export type SupportedLanguage = (typeof _supportedLanguages)[number];

// A class derived from the more generic I18n which requires II-supported languages to be implemented.
export class I18n extends BaseI18n<SupportedLanguage> {
  constructor() {
    super("en");
  }
}
