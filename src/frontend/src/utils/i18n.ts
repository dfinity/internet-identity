/** Internationalization (i18n) and copy support for Internet Identity.
*
* This module provides helper for loading multi-language copy definitions and updating the lit templates
* depending on the user-selected language dynamically. */

import { TemplateResult, html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { Chan } from "./utils";

// The type of raw copy, i.e. { lang: { some_key_title: "Welcome!" } }
type StringCopy<Keys extends string, Lang extends string> = {
  [key in Lang]: { [key in Keys]: string };
};

// The dynamic templates, i.e. { some_key_title: <language-dependent template> }
type DynamicCopy<Keys extends string> = { [key in Keys]: TemplateResult };

/// A class that converts a string copy definition into dynamic lit templates dependent on the
// currently selected language.
export class I18n<Lang extends string> {

  // The 'Chan' used to propagate language updates
  private chan: Chan<Lang>;

  // The currently selected language
  private lang: Lang;

  constructor(def: Lang) {
    this.chan = new Chan(def);
    this.lang = def;
  }

  i18n<Keys extends string>(copy: StringCopy<Keys, Lang>): DynamicCopy<Keys> {


    // Use the default language as the definition for expected keys.
    // Missing keys in other languages are a type error.
    type DefaultCopy = StringCopy<Keys, Lang>[Lang];
    const defCopy: DefaultCopy = copy[this.lang];
    const keys: [Keys] = Object.keys(defCopy) as [Keys];

    // Create dynamic/async templates for all languages
    const internationalized = keys.reduce((acc, k) => {
      const value: AsyncIterable<string> = this.chan.map((lang) => {
        return copy[lang][k];
      });

      acc[k] = html`${asyncReplace(value)}`;
      return acc;
    }, {} as DynamicCopy<Keys>);

    return internationalized;
  }

  // Set the language, causing all templates to update
  setLanguage(lang: Lang) {
    this.chan.send(lang);
    this.lang = lang;
  }

  // Retrieve the currently selected language
  getLanguageAsync(): AsyncIterable<Lang> {
    return this.chan.map((x) => x);
  }
}
