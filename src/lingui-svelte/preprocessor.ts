import { parse } from "svelte/compiler";
import { Plugin } from "vite";
import { walk, Node } from "estree-walker";
import MagicString from "magic-string";
import {
  findTransInCallExpression,
  findTransInTaggedTemplate,
  findPluralInCallExpression,
  FoundMessage,
  findTransInComponent,
  isWithinRanges,
  MESSAGE_TAGS,
  Range,
} from "./utils";

const overwriteCall = (
  isBuild: boolean,
  magicString: MagicString,
  msg: FoundMessage,
) => {
  // Include id in both development and build so translations can be found
  let output = `${msg.tag}({ id: ${JSON.stringify(msg.id)}`;
  // Include message during development so latest is shown,
  // exclude during build to optimize the total bundle size.
  if (!isBuild && msg.message != null) {
    output += `, message: ${JSON.stringify(msg.message)}`;
  }
  // Include values if they're found
  if (msg.values != null) {
    const map = Object.entries(msg.values)
      .map(([key, { start, end }]) => {
        const value = magicString.slice(start, end);
        return key === value ? key : `${key}: ${value}`;
      })
      .join(", ");
    output += `, values: { ${map} }`;
  }
  output += ` })`;
  magicString.overwrite(msg.start, msg.end, output);
};

const overwriteComponent = (
  isBuild: boolean,
  magicString: MagicString,
  msg: FoundMessage,
) => {
  // Include id in both development and build so translations can be found
  let output = `<${msg.tag} id={${JSON.stringify(msg.id)}}`;
  // Include message during development so latest is shown,
  // exclude during build to optimize the total bundle size.
  if (!isBuild) {
    output += ` message={${JSON.stringify(msg.message)}}`;
  }
  // Include values if they're found
  if (msg.values) {
    const map = Object.entries(msg.values)
      .map(([key, { start, end }]) => {
        const value = magicString.slice(start, end);
        return key === value ? key : `${key}: ${value}`;
      })
      .join(", ");
    output += ` values={{ ${map} }}`;
  }
  output += `>`;

  // Include nodes if they're found
  if (msg.nodes && msg.nodes.length > 0) {
    output += "{#snippet renderNode(__children, __index)}";
    msg.nodes.forEach(({ node, content }, idx) => {
      output += `{#if __index === ${idx}}`;
      if (content) {
        // Slice by original offsets rather than indexing into the node's own
        // text: a message rewritten inside this node, such as a `$t` in one of
        // its attributes, has already changed that text's length.
        output += magicString.slice(node.start, content.start);
        output += "{@render __children()}";
        output += magicString.slice(content.end, node.end);
      } else {
        output += magicString.slice(node.start, node.end);
      }

      output += "{/if}";
    });
    output += "{/snippet}";
  }

  output += `</${msg.tag}>`;
  magicString.overwrite(msg.start, msg.end, output);
};

export const svelteTransform = (isBuild: boolean, code: string) => {
  const magicString = new MagicString(code);
  const ast = parse(code, { modern: true });

  // Collect top-down, so an enclosing message is seen before the message
  // formats nested in it and can declare their ranges already carried.
  const found: Array<{ msg: FoundMessage; isComponent: boolean }> = [];
  const consumed: Range[] = [];
  const collect = (isComponent: boolean) => (msg: FoundMessage) => {
    if (msg.consumed) consumed.push(...msg.consumed);
    found.push({ msg, isComponent });
  };

  walk(ast as unknown as Node, {
    enter(node) {
      if (isWithinRanges(node, consumed)) return;
      findTransInTaggedTemplate(MESSAGE_TAGS, node, collect(false));
      findTransInCallExpression(MESSAGE_TAGS, node, collect(false));
      findPluralInCallExpression(MESSAGE_TAGS, node, collect(false));
      findTransInComponent(MESSAGE_TAGS, node, collect(true));
    },
  });

  // Rewrite right-to-left so a message nested inside another node's range —
  // a `$t` in an attribute of a `<Trans>` child, say — is already rewritten
  // by the time the enclosing range is sliced and overwritten.
  found
    .sort((a, b) => b.msg.start - a.msg.start)
    .forEach(({ msg, isComponent }) =>
      isComponent
        ? overwriteComponent(isBuild, magicString, msg)
        : overwriteCall(isBuild, magicString, msg),
    );

  return {
    code: magicString.toString(),
    map: magicString.generateMap({ hires: true }),
  };
};

export const sveltePreprocessor = (): Plugin => {
  let isBuild = false;
  return {
    name: "lingui-svelte-preprocessor",
    enforce: "pre",
    configResolved(config) {
      isBuild = config.command === "build";
    },
    transform(code, id) {
      if (!id.endsWith(".svelte")) {
        return;
      }
      return svelteTransform(isBuild, code);
    },
  };
};
