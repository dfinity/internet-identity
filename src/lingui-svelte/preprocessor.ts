import { parse } from "svelte/compiler";
import { Plugin } from "vite";
import { walk, Node } from "estree-walker";
import MagicString from "magic-string";
import {
  COMPONENT_FORMATS,
  findTransInCallExpression,
  findTransInTaggedTemplate,
  findPluralInCallExpression,
  FoundMessage,
  findTransInComponent,
  isWithinRanges,
  MessageFormats,
  Range,
} from "./utils";
import { findModuleFormats, isModule, parseModule } from "./module";

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

const transform = (
  isBuild: boolean,
  code: string,
  ast: unknown,
  formats: MessageFormats,
  isComponent: boolean,
) => {
  const magicString = new MagicString(code);

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
      findTransInTaggedTemplate(formats, node, collect(false));
      findTransInCallExpression(formats, node, collect(false));
      findPluralInCallExpression(formats, node, collect(false));
      if (isComponent) {
        findTransInComponent(["Trans"], node, collect(true));
      }
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

export const svelteTransform = (isBuild: boolean, code: string) =>
  transform(
    isBuild,
    code,
    parse(code, { modern: true }),
    COMPONENT_FORMATS,
    true,
  );

/** Undefined for a module that imports neither `t` nor `plural`. */
export const moduleTransform = (isBuild: boolean, code: string) => {
  const program = parseModule(code);
  const formats = findModuleFormats(program);
  if (formats === undefined) {
    return undefined;
  }
  return transform(isBuild, code, program, formats, false);
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
      if (id.endsWith(".svelte")) {
        return svelteTransform(isBuild, code);
      }
      if (isModule(id) && !id.includes("/node_modules/")) {
        return moduleTransform(isBuild, code);
      }
    },
  };
};
