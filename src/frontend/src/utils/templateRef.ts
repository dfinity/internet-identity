import { TemplateResult, render } from "lit-html";
import { Ref } from "lit-html/directives/ref.js";

/** This module contains some helpers that bring element lookup closer to the template description.
 * By using "lit-html" `Ref`s, DOM elements can be bound without needing `id`s. Then, when the `TemplateRef`
 * is rendered using `renderTemplateRef`, references are resolved returned.
 *
 * This means that, assuming references are given the proper types, element creation
 * is completely type-safe.
 */

/** A helper type that represents a "lit-html" template that contains (typed) references. */
export type TemplateRef<T> = {
  template: TemplateResult;
  refs: { [Property in keyof T]: Ref };
};

/** A wrapper around "lit-html"'s `render` that (blindly) reifies the template references */
export function renderTemplateRef<T>(
  tpl: TemplateRef<T>,
  container: HTMLElement
): T {
  render(tpl.template, container);

  // XXX: there may be a way to describe what's happening to tsc but I
  // haven't found it.
  // eslint-disable-next-line
  const ret = {} as any;

  for (const i in tpl.refs) {
    const val = tpl.refs[i].value;
    if (val === null) {
      throw Error("Bad reference found in template");
    }

    ret[i] = val;
  }

  return ret;
}
