import { Parser, type Program } from "acorn";
import { tsPlugin } from "@sveltejs/acorn-typescript";
import { analyze } from "eslint-scope";
import type { Identifier, Program as EstreeProgram } from "estree";
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
    // Scope analysis reads `range`
    ranges: true,
  });

/**
 * Matches the identifiers that resolve to the `t` and `plural` bindings
 * imported from the locale store, so a renamed import is followed and a
 * parameter or declaration shadowing one is not. Undefined when the module
 * imports neither and has nothing to translate.
 */
export const findModuleFormats = (
  program: Program,
): MessageFormats | undefined => {
  const references = {
    t: new Set<Identifier>(),
    plural: new Set<Identifier>(),
  };
  const scopeManager = analyze(program as unknown as EstreeProgram, {
    ecmaVersion: 2022,
    sourceType: "module",
  });
  const moduleScope = scopeManager.scopes.find(
    (scope) => scope.type === "module",
  );
  for (const variable of moduleScope?.variables ?? []) {
    for (const def of variable.defs) {
      if (
        def.type !== "ImportBinding" ||
        def.parent.source.value !== LOCALE_STORE ||
        def.node.type !== "ImportSpecifier" ||
        def.node.imported.type !== "Identifier"
      ) {
        continue;
      }
      const format = def.node.imported.name;
      if (format !== "t" && format !== "plural") {
        continue;
      }
      for (const reference of variable.references) {
        references[format].add(reference.identifier as Identifier);
      }
    }
  }
  if (references.t.size === 0 && references.plural.size === 0) {
    return undefined;
  }
  return {
    t: (identifier) => references.t.has(identifier),
    plural: (identifier) => references.plural.has(identifier),
  };
};
