import { parse } from "svelte/compiler";
import { walk, Node } from "estree-walker";
import { ExtractorType } from "@lingui/conf";
import {
  COMPONENT_FORMATS,
  findPluralInCallExpression,
  findTransInCallExpression,
  findTransInComponent,
  findTransInTaggedTemplate,
  FoundMessage,
  isWithinRanges,
  Range,
} from "./utils";
import { LinesAndColumns } from "lines-and-columns";
import { findModuleFormats, isModule, parseModule } from "./module";

export const svelteExtractor: ExtractorType = {
  match(filename) {
    return filename.endsWith(".svelte") || isModule(filename);
  },
  async extract(filename, source, onMessageExtracted, _ctx) {
    try {
      const isComponent = filename.endsWith(".svelte");
      const program = isComponent ? undefined : parseModule(source);
      const ast = program ?? parse(source, { filename, modern: true });
      const formats =
        program === undefined ? COMPONENT_FORMATS : findModuleFormats(program);
      if (formats === undefined) return;
      const lines = new LinesAndColumns(source);
      // Message formats nested inside another are already part of the
      // enclosing message. The walk is top-down, so an enclosing message is
      // always seen first and records the ranges to leave alone.
      const consumed: Range[] = [];
      // Only forward properties defined in `ExtractedMessage`
      const onMessageFound = ({
        id,
        message,
        context,
        comment,
        placeholders,
        start,
        consumed: absorbed,
      }: FoundMessage) => {
        if (absorbed) consumed.push(...absorbed);
        const { line, column } = lines.locationForIndex(start)!;
        onMessageExtracted({
          id,
          message,
          context,
          origin: [filename, line + 1, column],
          comment,
          placeholders,
        });
      };
      walk(ast as unknown as Node, {
        enter(node) {
          if (isWithinRanges(node, consumed)) return;
          findTransInTaggedTemplate(formats, node, onMessageFound);
          findTransInCallExpression(formats, node, onMessageFound);
          findPluralInCallExpression(formats, node, onMessageFound);
          if (isComponent) {
            findTransInComponent(["Trans"], node, onMessageFound);
          }
        },
      });
    } catch (err) {
      console.error(`Error at ${filename}:`, err);
    }
  },
};
