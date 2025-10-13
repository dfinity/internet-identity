import { parse } from "svelte/compiler";
import { walk, Node } from "estree-walker";
import { ExtractorType } from "@lingui/conf";
import {
  findPluralInCall,
  findTransInCall,
  findTransInComponent,
  findTransInTaggedTemplate,
  FoundMessage,
} from "./utils";

export const svelteExtractor: ExtractorType = {
  match(filename) {
    return filename.endsWith(".svelte");
  },
  async extract(filename, source, onMessageExtracted, _ctx) {
    try {
      const ast = parse(source, { filename, modern: true });
      // Only forward properties defined in `ExtractedMessage`
      const onMessageFound = ({
        id,
        message,
        context,
        origin,
        comment,
        placeholders,
      }: FoundMessage) =>
        onMessageExtracted({
          id,
          message,
          context,
          origin,
          comment,
          placeholders,
        });
      walk(ast as unknown as Node, {
        enter(node) {
          findTransInTaggedTemplate(["$t"], node, filename, onMessageFound);
          findTransInCall(["$t"], node, filename, onMessageFound);
          findPluralInCall(["$plural"], node, filename, onMessageFound);
          findTransInComponent(["Trans"], node, filename, onMessageFound);
        },
      });
    } catch (err) {
      console.error(`Error at ${filename}:`, err);
    }
  },
};
