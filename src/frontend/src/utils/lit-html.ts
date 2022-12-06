/* A couple of lit-html helpers */

import { Ref, ref } from "lit-html/directives/ref.js";
import { DirectiveResult } from "lit-html/directive.js";

// Read a "lit-html" ref, showing an error message (in the console) in case the
// element is not available.
export function withRef<A, B>(ref: Ref<A>, f: (val: A) => B): B | undefined {
  const value = ref.value;

  if (value === undefined) {
    console.error(
      "Internet Identity: Tried to access a DOM element that doesn't exist, this is a bug"
    );
    return;
  } else {
    return f(value);
  }
}

/* A lit-html directive that performs an action when the element is added
 * to the DOM.
 *
 * Note: there are no guarantees that the callback will be called as the
 * implementation relies on internal "lit-html" behavior. No critical behavior
 * should depend on this.
 */
export const mount = (callback: (elem: Element) => void): DirectiveResult =>
  ref((e: Element | undefined) => {
    if (e !== undefined) {
      // This works by observing the entire document for mutations, under
      // the assumption that the first DOM mutation to happen after the element
      // was created is inserting the element (or its parent) in the DOM. This
      // happens in practice but may change depending on what lit-html does.
      //
      // Note: The reason why we only observe exactly one mutation and then
      // disconnect is to avoid leaking the observer to keep observing the
      // DOM if the element was created but never mounted.
      //
      // Note: it would be much easier to use the "DOMNodeInsertedIntoDocument"
      // event on the element itself, but the API is deprecated and Firefox
      // does not support it.
      const observer = new MutationObserver(() => {
        try {
          // check that the element is indeed in the DOM and call callback
          if (e.isConnected) {
            callback(e);
          }
        } finally {
          observer.disconnect();
        }
      });

      observer.observe(document, { childList: true, subtree: true });
    }
  });

/* A lit-html directive that focuses the element when the element is added
 * to the DOM.
 */
export const autofocus = mount((elem: Element) => {
  if (elem instanceof HTMLElement) {
    elem.focus();
  }
});
