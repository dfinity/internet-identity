import { parse } from "svelte/compiler";
import { walk, Node } from "estree-walker";
import { ExtractorType } from "@lingui/conf";
import {
  findPluralInCallExpression,
  findTransInCallExpression,
  findTransInComponent,
  findTransInTaggedTemplate,
  FoundMessage,
} from "./utils";
import { LinesAndColumns } from "lines-and-columns";

export const svelteExtractor: ExtractorType = {
  match(filename) {
    return filename.endsWith(".svelte");
  },
  async extract(filename, source, onMessageExtracted, _ctx) {
    try {
      const ast = parse(source, { filename, modern: true });
      const lines = new LinesAndColumns(source);
      // Only forward properties defined in `ExtractedMessage`
      const onMessageFound = ({
        id,
        message,
        context,
        comment,
        placeholders,
        start,
      }: FoundMessage) => {
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
          findTransInTaggedTemplate(["$t"], node, onMessageFound);
          findTransInCallExpression(["$t"], node, onMessageFound);
          findPluralInCallExpression(["$plural"], node, onMessageFound);
          findTransInComponent(["Trans"], node, onMessageFound);
        },
      });
    } catch (err) {
      console.error(`Error at ${filename}:`, err);
    }
  },
};
