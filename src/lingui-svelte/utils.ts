import { AST } from "svelte/compiler";
import { Node } from "estree-walker";
import {
  Property,
  SpreadElement,
  TemplateLiteral,
  Identifier,
  Literal,
} from "estree";
import { createHash } from "crypto";
import { ExtractedMessage } from "@lingui/conf";

const UNIT_SEPARATOR = "\u001F";

export const generateMessageId = (message: string, context = "") =>
  createHash("sha256")
    .update(message + UNIT_SEPARATOR + context)
    .digest("base64")
    .slice(0, 6);

export interface FoundMessage extends Omit<ExtractedMessage, "origin"> {
  tag: string;
  start: number;
  end: number;
  values?: Record<string, { start: number; end: number }>;
  nodes?: Array<{
    node: { start: number; end: number };
    text?: { start: number; end: number };
  }>;
}

const findPropertyTemplateLiteral = (
  properties: (Property | SpreadElement)[],
  keyName: string,
): TemplateLiteral | undefined => {
  const prop = properties.find(
    (p): p is Property & { key: Identifier; value: TemplateLiteral } =>
      p.type === "Property" &&
      p.key.type === "Identifier" &&
      p.key.name === keyName &&
      p.value.type === "TemplateLiteral",
  );
  return prop?.value;
};

type StringLiteral = Literal & { value: string };
const findPropertyStringLiteral = (
  properties: (Property | SpreadElement)[],
  keyName: string,
): StringLiteral | undefined => {
  const prop = properties.find(
    (p): p is Property & { key: Identifier; value: Literal } =>
      p.type === "Property" &&
      p.key.type === "Identifier" &&
      p.key.name === keyName &&
      p.value.type === "Literal" &&
      typeof p.value.value === "string",
  );
  return prop?.value as StringLiteral;
};

const processTemplateLiteral = (node: TemplateLiteral) => {
  const rawQuasis = node.quasis.map((q) => q.value.raw);
  let message = rawQuasis[0];
  let values: Record<string, { start: number; end: number }> = {};

  let positionalCount = 0;
  rawQuasis.slice(1).forEach((q, i) => {
    const key =
      node.expressions[i].type === "Identifier"
        ? node.expressions[i].name
        : `${positionalCount++}`;
    message += `{${key}}${q}`;
    if (
      "start" in node.expressions[i] &&
      "end" in node.expressions[i] &&
      typeof node.expressions[i].start === "number" &&
      typeof node.expressions[i].end === "number"
    ) {
      values[key] = {
        start: node.expressions[i].start,
        end: node.expressions[i].end,
      };
    }
  });

  return { message, values };
};

export const findTransInTaggedTemplate = (
  tags: string[],
  node: Node,
  onMessageFound: (msg: FoundMessage) => void,
) => {
  if (
    node.type !== "TaggedTemplateExpression" ||
    node.tag.type !== "Identifier" ||
    !tags.includes(node.tag.name) ||
    !hasNumericStartEnd(node) ||
    node.quasi.loc == null
  ) {
    return;
  }

  const { message, values } = processTemplateLiteral(node.quasi);

  onMessageFound({
    tag: node.tag.name,
    id: generateMessageId(message),
    message,
    values: Object.keys(values).length > 0 ? values : undefined,
    start: node.start,
    end: node.end,
  });
};

export const findTransInCallExpression = (
  tags: string[],
  node: Node,
  onMessageFound: (msg: FoundMessage) => void,
) => {
  if (
    node.type !== "CallExpression" ||
    node.callee.type !== "Identifier" ||
    !tags.includes(node.callee.name) ||
    node.arguments[0].type !== "ObjectExpression" ||
    !hasNumericStartEnd(node) ||
    !node.loc
  ) {
    return;
  }

  const { properties } = node.arguments[0];

  const messageNode =
    findPropertyStringLiteral(properties, "message") ??
    findPropertyTemplateLiteral(properties, "message");
  if (!messageNode) return;

  const { message, values } =
    messageNode.type === "TemplateLiteral"
      ? processTemplateLiteral(messageNode)
      : { message: messageNode.value, values: {} };

  const id = findPropertyStringLiteral(properties, "id")?.value;
  const context = findPropertyStringLiteral(properties, "context")?.value;
  const comment = findPropertyStringLiteral(properties, "comment")?.value;

  onMessageFound({
    tag: node.callee.name,
    id: id ?? generateMessageId(message, context),
    message,
    context,
    comment,
    values: Object.keys(values).length > 0 ? values : undefined,
    start: node.start,
    end: node.end,
  });
};

export const findPluralInCallExpression = (
  tags: string[],
  node: Node,
  onMessageFound: (msg: FoundMessage) => void,
) => {
  if (
    node.type !== "CallExpression" ||
    node.callee.type !== "Identifier" ||
    !tags.includes(node.callee.name) ||
    node.arguments[1].type !== "ObjectExpression" ||
    !hasNumericStartEnd(node) ||
    !hasNumericStartEnd(node.arguments[0]) ||
    !node.loc
  ) {
    return;
  }

  const num =
    node.arguments[0].type === "Identifier" ? node.arguments[0].name : "num";
  let values: Record<string, { start: number; end: number }> = {
    [num]: { start: node.arguments[0].start, end: node.arguments[0].end },
  };

  const pluralParts = node.arguments[1].properties.flatMap((p) => {
    if (
      p.type !== "Property" ||
      !(p.key.type === "Identifier" || p.key.type === "Literal") ||
      !(p.value.type === "Literal" || p.value.type === "TemplateLiteral")
    )
      return [];

    const keyStr =
      p.key.type === "Identifier" ? p.key.name : String(p.key.value);

    if (p.value.type === "TemplateLiteral") {
      const { message: valueMessage, values: valueValues } =
        processTemplateLiteral(p.value);
      values = { ...values, ...valueValues };
      return [`${keyStr} {${valueMessage}}`];
    }

    return [`${keyStr} {${p.value.value}}`];
  });

  if (!pluralParts.length) return;

  const message = `{${num}, plural, ${pluralParts.join(" ")}}`;

  onMessageFound({
    tag: node.callee.name,
    id: generateMessageId(message),
    message,
    values: Object.keys(values).length > 0 ? values : undefined,
    start: node.start,
    end: node.end,
  });
};

const hasNumericStartEnd = (
  node: any,
): node is { start: number; end: number } =>
  node && typeof node.start === "number" && typeof node.end === "number";

const isComponent = (node: unknown): node is AST.Component =>
  typeof node === "object" &&
  node != null &&
  "type" in node &&
  node.type === "Component";

const textInNode = (
  node: AST.TemplateNode,
  valueCount: { count: number },
  register: (entry: {
    node: { start: number; end: number };
    text?: { start: number; end: number };
  }) => number,
): {
  text: string;
  start: number;
  end: number;
  values: Record<string, { start: number; end: number }>;
  registered: boolean;
} => {
  // Text node
  if (node.type === "Text") {
    return {
      text: node.data
        .replace(/[\r\n]+/g, "")
        .replace(/^\s+/, " ")
        .replace(/\s+$/, " "),
      start: node.start,
      end: node.end,
      values: {},
      registered: false,
    };
  }

  // Expression tag (mustache)
  if (node.type === "ExpressionTag" && hasNumericStartEnd(node.expression)) {
    const key =
      node.expression.type === "Identifier"
        ? node.expression.name
        : valueCount.count++;
    return {
      text: `{${key}}`,
      start: node.start,
      end: node.end,
      values: {
        [key]: { start: node.expression.start, end: node.expression.end },
      },
      registered: false,
    };
  }

  // Non-fragment nodes
  if (
    !("fragment" in node) ||
    !node.fragment ||
    !Array.isArray(node.fragment.nodes)
  ) {
    return {
      text: "",
      start: node.start ?? 0,
      end: node.end ?? 0,
      values: {},
      registered: false,
    };
  }

  // Recurse into children
  const childResults = node.fragment.nodes.map((child) =>
    textInNode(child, valueCount, register),
  );

  const combinedText = childResults.map((c) => c.text).join("");
  const combinedValues = childResults.reduce(
    (acc, c) => ({ ...acc, ...c.values }),
    {},
  );

  const firstChild = childResults[0];
  const lastChild = childResults[childResults.length - 1];
  const start = firstChild?.start ?? node.start;
  const end = lastChild?.end ?? node.end;
  const trimmedInner = combinedText.replace(/[\r\n]+/g, "").trim();

  // Register this node
  const idx = register({
    node: { start: node.start, end: node.end },
    text: trimmedInner.length > 0 ? { start, end } : undefined,
  });

  return {
    text:
      trimmedInner.length > 0
        ? `<${idx}>${trimmedInner}</${idx}>`
        : `<${idx}/>`,
    start,
    end,
    values: combinedValues,
    registered: true,
  };
};

export const findTransInComponent = (
  tags: string[],
  node: Node,
  onMessageFound: (msg: FoundMessage) => void,
) => {
  const component = node as unknown;
  if (!isComponent(component) || !tags.includes(component.name)) {
    return;
  }

  const valueCount = { count: 0 };
  const nodes: Array<{
    node: { start: number; end: number };
    text?: { start: number; end: number };
  }> = [];

  // Helper to register a node and return its index
  const register = (entry: {
    node: { start: number; end: number };
    text?: { start: number; end: number };
  }) => nodes.push(entry) - 1;

  let message = "";
  let values: Record<string, { start: number; end: number }> = {};

  for (const child of component.fragment.nodes) {
    // Text node
    if (child.type === "Text") {
      message += child.data
        .replace(/[\r\n]+/g, "")
        .replace(/^\s+/, " ")
        .replace(/\s+$/, " ");
      continue;
    }

    // Expression tag / mustache
    if (
      child.type === "ExpressionTag" &&
      "start" in child.expression &&
      typeof child.expression.start === "number" &&
      "end" in child.expression &&
      typeof child.expression.end === "number"
    ) {
      const key =
        child.expression.type === "Identifier"
          ? child.expression.name
          : valueCount.count++;
      message += `{${key}}`;
      values[key] = {
        start: child.expression.start,
        end: child.expression.end,
      };
      continue;
    }

    // Element / component node
    const res = textInNode(child as AST.TemplateNode, valueCount, register);
    values = { ...values, ...res.values };

    const text = res.text.replace(/[\r\n]+/g, "").trim();
    message += text.length > 0 ? text : res.text;
  }
  message = message.trim();

  // Extract optional attributes for id/context/comment
  const getAttr = (name: string) =>
    (
      component.attributes.find(
        (attr): attr is AST.Attribute =>
          attr.type === "Attribute" && attr.name === name,
      )?.value as AST.Text[]
    )?.[0]?.data;

  const id = getAttr("id");
  const context = getAttr("context");
  const comment = getAttr("comment");

  onMessageFound({
    tag: component.name,
    id: id ?? generateMessageId(message, context),
    message,
    context,
    comment,
    values: Object.keys(values).length > 0 ? values : undefined,
    nodes: nodes.length > 0 ? nodes : undefined,
    start: component.start,
    end: component.end,
  });
};
