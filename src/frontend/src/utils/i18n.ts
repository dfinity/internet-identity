import { TemplateResult, html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { Chan } from "./utils";

// TODO: custom type errors when missing in language?

type Language = typeof languages[number];

const languages = ["en"] as const;

type StringCopy<Keys extends string> = {
  [key in Language]: { [key in Keys]: string };
};
type DynamicCopy<Keys extends string> = { [key in Keys]: TemplateResult };

export interface I18n {
  setLanguage: (lang: Language) => void;
  i18n: <Keys extends string>(copy: StringCopy<Keys>) => DynamicCopy<Keys>;
}

export class LocalStorageI18n implements I18n {
  // TODO: support deregistering
  private chan: Chan<Language> = new Chan(LocalStorageI18n.inferUserLanguage());

  static inferUserLanguage(): Language {
    const lsLang = window.localStorage.getItem("lang") as Language;
    if (lsLang !== null && languages.includes(lsLang)) {
      return lsLang as Language; // TODO check this
    }
    return "en";
  }

  i18n<Keys extends string>(copy: StringCopy<Keys>): DynamicCopy<Keys> {
    type EnglishCopy = StringCopy<Keys>["en"];
    const englishCopy: EnglishCopy = copy.en;
    const keys: [Keys] = Object.keys(englishCopy) as [Keys];

    const internationalized = keys.reduce((acc, k) => {
      const value: AsyncIterable<string> = this.chan.map((lang) => {
        return copy[lang][k];
      });

      acc[k] = html`${asyncReplace(value)}`;
      return acc;
    }, {} as DynamicCopy<Keys>);

    return internationalized;
  }

  setLanguage(lang: Language) {
    this.chan.send(lang);
    window.localStorage.setItem("lang", lang);
  }
}

export class DummyI18n implements I18n {
  // TODO: support deregistering
  private chan: Chan<Language> = new Chan("en");

  i18n<Keys extends string>(copy: StringCopy<Keys>): DynamicCopy<Keys> {
    type EnglishCopy = StringCopy<Keys>["en"];
    const englishCopy: EnglishCopy = copy.en;
    const keys: [Keys] = Object.keys(englishCopy) as [Keys];

    const internationalized = keys.reduce((acc, k) => {
      const value: AsyncIterable<string> = this.chan.map((lang) => {
        return copy[lang][k];
      });

      acc[k] = html`${asyncReplace(value)}`;
      return acc;
    }, {} as DynamicCopy<Keys>);

    return internationalized;
  }

  setLanguage(lang: Language) {
    console.log("Set language:", lang);
    this.chan.send(lang);
  }
}
