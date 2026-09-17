import { AST } from "svelte/compiler";
import { Node } from "estree-walker";
import {
  Property,
  SpreadElement,
  TemplateLiteral,
  Identifier,
  Literal,
  Expression,
  CallExpression,
} from "estree";
import { createHash } from "crypto";
import { ExtractedMessage } from "@lingui/conf";

const UNIT_SEPARATOR = "\u001F";

export const generateMessageId = (message: string, context = "") =>
  createHash("sha256")
    .update(message + UNIT_SEPARATOR + context)
    .digest("base64")
    .slice(0, 6);

export interface Range {
  start: number;
  end: number;
}

export interface FoundMessage extends Omit<ExtractedMessage, "origin"> {
  tag: string;
  start: number;
  end: number;
  values?: Record<string, Range>;
  nodes?: Array<{
    node: Range;
    content?: Range;
  }>;
  /**
   * Ranges of nested message formats whose text this message already carries.
   * A walker must not emit or rewrite them again — there is one catalogue
   * entry, and the outer rewrite replaces their source outright.
   */
  consumed?: Range[];
}

/** The names each message format goes by in source. */
export interface MessageTags {
  t: string[];
  plural: string[];
}

export const MESSAGE_TAGS: MessageTags & { trans: string[] } = {
  t: ["$t"],
  plural: ["$plural"],
  trans: ["Trans"],
};

interface ResolvedMessage {
  message: string;
  values: Record<string, Range>;
}

/**
 * Positional placeholder keys are unique within one message. Sibling plural
 * categories are alternatives, so each starts numbering where the enclosing
 * message left off and the enclosing message resumes past the highest key any
 * category reached.
 */
interface ResolveContext {
  positional: { next: number };
  consumed: Range[];
}

const createResolveContext = (): ResolveContext => ({
  positional: { next: 0 },
  consumed: [],
});

export const isWithinRanges = (node: unknown, ranges: Range[]): boolean =>
  hasNumericStartEnd(node) &&
  ranges.some((range) => node.start >= range.start && node.end <= range.end);

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

const processTemplateLiteral = (
  node: TemplateLiteral,
  tags: MessageTags,
  ctx: ResolveContext,
): ResolvedMessage => {
  const rawQuasis = node.quasis.map((q) => q.value.raw);
  let message = rawQuasis[0];
  const values: Record<string, Range> = {};

  rawQuasis.slice(1).forEach((q, i) => {
    const expression = node.expressions[i];

    const nested = resolveNestedMessage(expression, tags, ctx);
    if (nested) {
      message += `${nested.message}${q}`;
      Object.assign(values, nested.values);
      return;
    }

    const key =
      expression.type === "Identifier"
        ? expression.name
        : `${ctx.positional.next++}`;
    message += `{${key}}${q}`;
    if (hasNumericStartEnd(expression)) {
      values[key] = { start: expression.start, end: expression.end };
    }
  });

  return { message, values };
};

const findProperty = (
  properties: (Property | SpreadElement)[],
  keyName: string,
): Property | undefined =>
  properties.find(
    (p): p is Property & { key: Identifier } =>
      p.type === "Property" &&
      p.key.type === "Identifier" &&
      p.key.name === keyName,
  );

const propertyKeyName = (property: Property): string | undefined => {
  if (property.key.type === "Identifier") return property.key.name;
  if (property.key.type === "Literal") return String(property.key.value);
  return undefined;
};

/**
 * Builds the ICU plural from a `$plural(value, { …categories })` call. Each
 * category is resolved through `resolveMessage`, so a category may hold a
 * plain string, a template literal, or another message format.
 */
const resolvePlural = (
  node: CallExpression,
  tags: MessageTags,
  ctx: ResolveContext,
): ResolvedMessage | undefined => {
  const [value, options] = node.arguments;
  if (
    options?.type !== "ObjectExpression" ||
    !value ||
    !hasNumericStartEnd(value)
  ) {
    return undefined;
  }

  const num = value.type === "Identifier" ? value.name : "num";
  const values: Record<string, Range> = {
    [num]: { start: value.start, end: value.end },
  };

  // Categories are alternatives, so each numbers its positional placeholders
  // from the same starting point and the enclosing message resumes past the
  // highest any of them reached.
  const firstKey = ctx.positional.next;
  let lastKey = firstKey;

  const categories = options.properties.flatMap((property) => {
    if (property.type !== "Property") return [];
    const key = propertyKeyName(property);
    if (key === undefined) return [];

    ctx.positional.next = firstKey;
    const resolved = resolveMessage(property.value as Expression, tags, ctx);
    if (!resolved) return [];
    lastKey = Math.max(lastKey, ctx.positional.next);

    Object.assign(values, resolved.values);
    return [`${key} {${resolved.message}}`];
  });

  ctx.positional.next = lastKey;

  if (!categories.length) return undefined;

  return { message: `{${num}, plural, ${categories.join(" ")}}`, values };
};

/**
 * Resolves a nested message format — `$t\`…\``, `$t({ … })` or
 * `$plural(…)` — and records its range as consumed, since its text now
 * belongs to the enclosing message.
 *
 * Returns undefined for anything else. In an expression slot, such as a
 * template-literal interpolation or a `<Trans>` mustache, a plain string or
 * template is a runtime value and has to stay a placeholder; only a message
 * format contributes text of its own.
 */
const resolveNestedMessage = (
  node: Expression | Property["value"] | undefined,
  tags: MessageTags,
  ctx: ResolveContext,
): ResolvedMessage | undefined => {
  if (!node) return undefined;

  if (
    node.type === "TaggedTemplateExpression" &&
    node.tag.type === "Identifier" &&
    tags.t.includes(node.tag.name)
  ) {
    const resolved = processTemplateLiteral(node.quasi, tags, ctx);
    if (hasNumericStartEnd(node)) {
      ctx.consumed.push({ start: node.start, end: node.end });
    }
    return resolved;
  }

  if (node.type !== "CallExpression" || node.callee.type !== "Identifier") {
    return undefined;
  }

  const resolved = tags.plural.includes(node.callee.name)
    ? resolvePlural(node, tags, ctx)
    : tags.t.includes(node.callee.name)
      ? resolveDescriptor(node, tags, ctx)?.resolved
      : undefined;

  if (resolved && hasNumericStartEnd(node)) {
    ctx.consumed.push({ start: node.start, end: node.end });
  }
  return resolved;
};

/**
 * Resolves whatever stands where a message is expected — a plural category or
 * a descriptor's `message` — which is a string, a template literal, or another
 * message format.
 */
const resolveMessage = (
  node: Expression | Property["value"] | undefined,
  tags: MessageTags,
  ctx: ResolveContext,
): ResolvedMessage | undefined => {
  if (!node) return undefined;

  if (node.type === "Literal") {
    return typeof node.value === "string"
      ? { message: node.value, values: {} }
      : undefined;
  }

  if (node.type === "TemplateLiteral") {
    return processTemplateLiteral(node, tags, ctx);
  }

  return resolveNestedMessage(node, tags, ctx);
};

/**
 * Reads a `$t({ message, id, context, comment })` call. The descriptor fields
 * belong to the outermost call — a nested one contributes only its message,
 * since there is a single catalogue entry.
 */
const resolveDescriptor = (
  node: CallExpression,
  tags: MessageTags,
  ctx: ResolveContext,
):
  | {
      resolved: ResolvedMessage;
      id?: string;
      context?: string;
      comment?: string;
    }
  | undefined => {
  const [descriptor] = node.arguments;
  if (descriptor?.type !== "ObjectExpression") return undefined;

  const { properties } = descriptor;
  const message = findProperty(properties, "message")?.value;
  const resolved = resolveMessage(message as Expression, tags, ctx);
  if (!resolved) return undefined;

  return {
    resolved,
    id: findPropertyStringLiteral(properties, "id")?.value,
    context: findPropertyStringLiteral(properties, "context")?.value,
    comment: findPropertyStringLiteral(properties, "comment")?.value,
  };
};

export const findTransInTaggedTemplate = (
  tags: MessageTags,
  node: Node,
  onMessageFound: (msg: FoundMessage) => void,
) => {
  if (
    node.type !== "TaggedTemplateExpression" ||
    node.tag.type !== "Identifier" ||
    !tags.t.includes(node.tag.name) ||
    !hasNumericStartEnd(node) ||
    node.quasi.loc == null
  ) {
    return;
  }

  const ctx = createResolveContext();
  const { message, values } = processTemplateLiteral(node.quasi, tags, ctx);

  onMessageFound({
    tag: node.tag.name,
    id: generateMessageId(message),
    message,
    values: Object.keys(values).length > 0 ? values : undefined,
    consumed: ctx.consumed.length > 0 ? ctx.consumed : undefined,
    start: node.start,
    end: node.end,
  });
};

export const findTransInCallExpression = (
  tags: MessageTags,
  node: Node,
  onMessageFound: (msg: FoundMessage) => void,
) => {
  if (
    node.type !== "CallExpression" ||
    node.callee.type !== "Identifier" ||
    !tags.t.includes(node.callee.name) ||
    !hasNumericStartEnd(node) ||
    !node.loc
  ) {
    return;
  }

  const ctx = createResolveContext();
  const descriptor = resolveDescriptor(node, tags, ctx);
  if (!descriptor) return;

  const { resolved, id, context, comment } = descriptor;
  const { message, values } = resolved;

  onMessageFound({
    tag: node.callee.name,
    id: id ?? generateMessageId(message, context),
    message,
    context,
    comment,
    values: Object.keys(values).length > 0 ? values : undefined,
    consumed: ctx.consumed.length > 0 ? ctx.consumed : undefined,
    start: node.start,
    end: node.end,
  });
};

export const findPluralInCallExpression = (
  tags: MessageTags,
  node: Node,
  onMessageFound: (msg: FoundMessage) => void,
) => {
  if (
    node.type !== "CallExpression" ||
    node.callee.type !== "Identifier" ||
    !tags.plural.includes(node.callee.name) ||
    !hasNumericStartEnd(node) ||
    !node.loc
  ) {
    return;
  }

  const ctx = createResolveContext();
  const resolved = resolvePlural(node, tags, ctx);
  if (!resolved) return;

  const { message, values } = resolved;

  onMessageFound({
    tag: node.callee.name,
    id: generateMessageId(message),
    message,
    values: Object.keys(values).length > 0 ? values : undefined,
    consumed: ctx.consumed.length > 0 ? ctx.consumed : undefined,
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
  tags: MessageTags,
  ctx: ResolveContext,
  register: (entry: { node: Range; content?: Range }) => number,
): {
  text: string;
  start: number;
  end: number;
  values: Record<string, Range>;
  registered: boolean;
} => {
  if (node.type === "Text") {
    const trimmed = node.data.replace(/[\r\n]+/g, "").replace(/\s+/g, " ");
    return {
      text: trimmed,
      start: node.start,
      end: node.end,
      values: {},
      registered: false,
    };
  }

  if (node.type === "ExpressionTag" && hasNumericStartEnd(node.expression)) {
    const nested = resolveNestedMessage(node.expression, tags, ctx);
    if (nested) {
      return {
        text: nested.message,
        start: node.expression.start,
        end: node.expression.end,
        values: nested.values,
        registered: false,
      };
    }

    const key =
      node.expression.type === "Identifier"
        ? node.expression.name
        : `${ctx.positional.next++}`;
    return {
      text: `{${key}}`,
      start: node.expression.start,
      end: node.expression.end,
      values: {
        [key]: { start: node.expression.start, end: node.expression.end },
      },
      registered: false,
    };
  }

  if (
    !("fragment" in node) ||
    !node.fragment ||
    !Array.isArray(node.fragment.nodes)
  ) {
    return {
      text: "",
      start: node.start,
      end: node.end,
      values: {},
      registered: false,
    };
  }

  const childResults = node.fragment.nodes.map((child) =>
    textInNode(child, tags, ctx, register),
  );

  const combinedText = childResults
    .map((c) => c.text)
    .join("")
    .trim();
  const combinedValues = childResults.reduce(
    (acc, c) => ({ ...acc, ...c.values }),
    {},
  );

  const idx = register({
    node: { start: node.start, end: node.end },
    content:
      node.fragment.nodes.length > 0
        ? {
            start: node.fragment.nodes[0].start,
            end: node.fragment.nodes[node.fragment.nodes.length - 1].end,
          }
        : undefined,
  });

  return {
    text:
      combinedText.length > 0
        ? `<${idx}>${combinedText}</${idx}>`
        : `<${idx}/>`,
    start: node.start,
    end: node.end,
    values: combinedValues,
    registered: true,
  };
};

export const findTransInComponent = (
  tags: MessageTags & { trans: string[] },
  node: Node,
  onMessageFound: (msg: FoundMessage) => void,
) => {
  const component = node as unknown;
  if (!isComponent(component) || !tags.trans.includes(component.name)) {
    return;
  }

  const ctx = createResolveContext();
  const nodes: Array<{ node: Range; content?: Range }> = [];

  // Helper to register a node and return its index
  const register = (entry: { node: Range; content?: Range }) =>
    nodes.push(entry) - 1;

  let message = "";
  let values: Record<string, Range> = {};

  for (const child of component.fragment.nodes) {
    // Text node
    if (child.type === "Text") {
      message += child.data.replace(/[\r\n]+/g, "").replace(/\s+/g, " ");
      continue;
    }

    // Expression tag / mustache
    if (
      child.type === "ExpressionTag" &&
      hasNumericStartEnd(child.expression)
    ) {
      // A nested message format contributes its own text to this message
      // rather than a placeholder standing in for a runtime value.
      const nested = resolveNestedMessage(child.expression, tags, ctx);
      if (nested) {
        message += nested.message;
        values = { ...values, ...nested.values };
        continue;
      }

      const key =
        child.expression.type === "Identifier"
          ? child.expression.name
          : `${ctx.positional.next++}`;
      message += `{${key}}`;
      values[key] = {
        start: child.expression.start,
        end: child.expression.end,
      };
      continue;
    }

    // Element / component node
    const res = textInNode(child as AST.TemplateNode, tags, ctx, register);
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
    consumed: ctx.consumed.length > 0 ? ctx.consumed : undefined,
    start: component.start,
    end: component.end,
  });
};
