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
} from "./utils";

const overwriteCall = (
  isBuild: boolean,
  magicString: MagicString,
  msg: FoundMessage,
) => {
  // Include id in both development and build so translations can be found
  let output = `${msg.tag}({id:"${msg.id}"`;
  // Include message during development so latest is shown,
  // exclude during build to optimize the total bundle size.
  if (!isBuild) {
    output += `,message:"${msg.message}"`;
  }
  // Include values if they're found
  if (msg.values != null) {
    const map = Object.entries(msg.values)
      .map(([key, { start, end }]) => {
        const value = magicString.slice(start, end);
        return key === value ? key : `${key}:${value}`;
      })
      .join(",");
    output += `,values:{${map}}`;
  }
  output += `})`;
  magicString.overwrite(msg.start, msg.end, output);
};

const overwriteComponent = (
  isBuild: boolean,
  magicString: MagicString,
  msg: FoundMessage,
) => {
  // Include id in both development and build so translations can be found
  let output = `<${msg.tag} id="${msg.id}"`;
  // Include message during development so latest is shown,
  // exclude during build to optimize the total bundle size.
  if (!isBuild) {
    output += ` message={"${msg.message}"}`;
  }
  // Include values if they're found
  if (msg.values != null) {
    const map = Object.entries(msg.values)
      .map(([key, { start, end }]) => {
        const value = magicString.slice(start, end);
        return key === value ? key : `${key}:${value}`;
      })
      .join(",");
    output += ` values={{${map}}}`;
  }
  output += `>`;
  // Include nodes with a snippet if they're found
  if (msg.nodes != null && msg.nodes.length > 0) {
    output += "{#snippet renderNode(__children, __index)}";
    msg.nodes.forEach(({ node, text }, idx) => {
      output += `{#if __index === ${idx}}`;
      const nodeSrc = magicString.slice(node.start, node.end);
      if (text) {
        const inner = magicString.slice(text.start, text.end);
        const replaced = nodeSrc.replace(inner, "{@render __children()}");
        output += replaced;
      } else {
        output += nodeSrc;
      }
      output += "{/if}";
    });
    output += "{/snippet}";
  }
  output += `</${msg.tag}>`;
  magicString.overwrite(msg.start, msg.end, output);
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
      const magicString = new MagicString(code);
      const ast = parse(code, { modern: true });
      walk(ast as unknown as Node, {
        // Modify bottom-up to avoid overlap
        leave(node) {
          findTransInTaggedTemplate(["$t"], node, (msg) =>
            overwriteCall(isBuild, magicString, msg),
          );
          findTransInCallExpression(["$t"], node, (msg) =>
            overwriteCall(isBuild, magicString, msg),
          );
          findPluralInCallExpression(["$plural"], node, (msg) =>
            overwriteCall(isBuild, magicString, msg),
          );
          findTransInComponent(["Trans"], node, (msg) =>
            overwriteComponent(isBuild, magicString, msg),
          );
        },
      });
      return {
        code: magicString.toString(),
        map: magicString.generateMap({ hires: true }),
      };
    },
  };
};
