import { Parser, type Program } from "acorn";
import { tsPlugin } from "@sveltejs/acorn-typescript";
import { MessageFormats } from "./utils";

const LOCALE_STORE = "$lib/stores/locale.store";

const TypeScriptParser = Parser.extend(tsPlugin());

/** A `.ts` or `.svelte.ts` module, but not a declaration file. */
export const isModule = (filename: string): boolean =>
  filename.endsWith(".ts") && !filename.endsWith(".d.ts");

export const parseModule = (code: string): Program =>
  TypeScriptParser.parse(code, {
    ecmaVersion: "latest",
    sourceType: "module",
    locations: true,
  });

/**
 * The local names `t` and `plural` are imported under from the locale store,
 * or undefined when the module imports neither and has nothing to translate.
 */
export const findModuleFormats = (
  program: Program,
): MessageFormats | undefined => {
  const formats: MessageFormats = { t: [], plural: [] };
  for (const statement of program.body) {
    if (
      statement.type !== "ImportDeclaration" ||
      statement.source.value !== LOCALE_STORE
    ) {
      continue;
    }
    for (const specifier of statement.specifiers) {
      if (
        specifier.type !== "ImportSpecifier" ||
        specifier.imported.type !== "Identifier"
      ) {
        continue;
      }
      if (specifier.imported.name === "t") {
        formats.t.push(specifier.local.name);
      }
      if (specifier.imported.name === "plural") {
        formats.plural.push(specifier.local.name);
      }
    }
  }
  return formats.t.length > 0 || formats.plural.length > 0
    ? formats
    : undefined;
};
