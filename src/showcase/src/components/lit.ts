import { TemplateResult, html, render } from "lit-html";

/** A wrapper for lit-html components, so that they can be used
 * as webcomponents.
 *
 * To create a component, make the new class extend LitComponent and define the render() method
 * to return a TemplateResult.
 *
 * Then, add it to the browser's custom elements (customElements.define("my-com", ClassExtendingThis))
 */
export class LitComponent extends HTMLElement {
  constructor() {
    super();
    render(this.render(), this);
  }

  render(): TemplateResult {
    return html`Nothing to show`;
  }
}
