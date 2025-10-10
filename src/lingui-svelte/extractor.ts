// Copied and converted to TS from: https://github.com/HenryLie/svelte-i18n-lingui/blob/main/src/lib/extractor.js
import { parse } from "svelte/compiler";
import { walk, Node } from "estree-walker";
import {
  TaggedTemplateExpression,
  CallExpression,
  Property,
  Identifier,
  Literal,
  SpreadElement,
} from "estree";
import { ExtractedMessage, ExtractorType } from "@lingui/conf";
import { generateMessageId } from "./generateMessageId";

// Helper to safely get a literal string property by key name
const findPropertyLiteral = (
  properties: (Property | SpreadElement)[],
  keyName: string,
): string | undefined => {
  const prop = properties.find(
    (p): p is Property & { key: Identifier; value: Literal } =>
      p.type === "Property" &&
      p.key.type === "Identifier" &&
      p.key.name === keyName &&
      p.value.type === "Literal" &&
      typeof p.value.value === "string",
  );
  return typeof prop?.value.value === "string" ? prop.value.value : undefined;
};

// Extract messages from TaggedTemplateExpression ($t)
const extractFromTaggedTemplate = (
  node: TaggedTemplateExpression,
  filename: string,
  onMessageExtracted: (msg: ExtractedMessage) => void,
) => {
  if (node.quasi.loc == null) {
    return;
  }
  const { start } = node.quasi.loc;

  const rawQuasis = node.quasi.quasis.map((q) => q.value.raw);
  let message = rawQuasis[0];
  rawQuasis.slice(1).forEach((q, i) => {
    message += `{${i}}${q}`;
  });

  onMessageExtracted({
    id: generateMessageId(message),
    message,
    origin: [filename, start.line, start.column],
    placeholders: {},
  });
};

// Extract messages from CallExpression ($t)
const extractFromCallExpression = (
  node: CallExpression,
  filename: string,
  onMessageExtracted: (msg: ExtractedMessage) => void,
) => {
  if (!node.loc || node.arguments[0].type !== "ObjectExpression") return;
  const { start } = node.loc;
  const { properties } = node.arguments[0];

  const message = findPropertyLiteral(properties, "message");
  if (!message) return;

  const context = findPropertyLiteral(properties, "context");
  const comment = findPropertyLiteral(properties, "comment");

  onMessageExtracted({
    id: generateMessageId(message, context),
    message,
    context,
    comment,
    origin: [filename, start.line, start.column],
    placeholders: {},
  });
};

// Extract $t messages from TaggedTemplateExpression or CallExpression
const extractTags = (
  tags: string[],
  node: Node,
  filename: string,
  onMessageExtracted: (msg: ExtractedMessage) => void,
) => {
  if (
    node.type === "TaggedTemplateExpression" &&
    node.tag.type === "Identifier" &&
    tags.includes(node.tag.name)
  ) {
    extractFromTaggedTemplate(node, filename, onMessageExtracted);
  } else if (
    node.type === "CallExpression" &&
    node.callee.type === "Identifier" &&
    tags.includes(node.callee.name) &&
    node.arguments[0].type === "ObjectExpression"
  ) {
    extractFromCallExpression(node, filename, onMessageExtracted);
  }
};

// Extract plural messages ($plural)
const extractPlurals = (
  tags: string[],
  node: Node,
  filename: string,
  onMessageExtracted: (msg: ExtractedMessage) => void,
) => {
  if (
    node.type === "CallExpression" &&
    node.callee.type === "Identifier" &&
    tags.includes(node.callee.name) &&
    node.arguments[1]?.type === "ObjectExpression" &&
    node.loc != null
  ) {
    const { start } = node.loc;
    const { properties } = node.arguments[1];

    const pluralParts = properties.flatMap((p) => {
      if (
        p.type !== "Property" ||
        !(p.key.type === "Identifier" || p.key.type === "Literal") ||
        p.value.type !== "Literal"
      )
        return [];
      const keyStr =
        p.key.type === "Identifier" ? p.key.name : String(p.key.value);
      const valueStr = String(p.value.value);
      return [`${keyStr} {${valueStr}}`];
    });

    if (pluralParts.length === 0) return;

    const message = `{num, plural, ${pluralParts.join(" ")}}`;

    onMessageExtracted({
      id: generateMessageId(message),
      message,
      origin: [filename, start.line, start.column],
      placeholders: {},
    });
  }
};

// Svelte extractor definition
export const svelteExtractor: ExtractorType = {
  match(filename) {
    return filename.endsWith(".svelte");
  },
  async extract(filename, source, onMessageExtracted, _ctx) {
    try {
      const ast = parse(source, { filename, modern: true });
      walk(ast as unknown as Node, {
        enter(node) {
          extractTags(["$t"], node, filename, onMessageExtracted);
          extractPlurals(["$plural"], node, filename, onMessageExtracted);
        },
      });
    } catch (err) {
      console.error(`Error at ${filename}:`, err);
    }
  },
};
