import { PageObjectElement } from "../../types/pageObject";
import { isNullish } from "@dfinity/utils";

export class WebDriverPageObjectElement implements PageObjectElement {
  readonly #browser: WebdriverIO.Browser;
  readonly #selectElement: () => Promise<WebdriverIO.Element | undefined>;
  #element?: Promise<WebdriverIO.Element>;

  constructor(
    browser: WebdriverIO.Browser,
    selectElement: () => Promise<WebdriverIO.Element | undefined>,
  ) {
    this.#browser = browser;
    this.#selectElement = selectElement;
  }

  static create(browser: WebdriverIO.Browser): PageObjectElement {
    return new WebDriverPageObjectElement(browser, () => browser.$("body"));
  }

  #getElement(): Promise<WebdriverIO.Element> {
    if (isNullish(this.#element)) {
      this.#element = this.#browser
        .waitUntil(this.#selectElement, {
          timeout: 10000,
        })
        .then((element) => element!);
    }
    return this.#element;
  }

  getByRole(role: string, { name }: { name: string }): PageObjectElement {
    return new WebDriverPageObjectElement(this.#browser, async () => {
      // WebdriverIO doesn't support combining both text and role,
      // so instead we select all text matches and then check for
      // the matching role with the `getComputedRole` method.
      const elements = await (await this.#getElement()).$$(`aria/${name}`);
      for (const element of elements) {
        try {
          if ((await element.getComputedRole()) === role) {
            return element;
          } else if (
            (await element.parentElement().getComputedRole()) === role
          ) {
            // `aria/Name of button` unfortunately selects the span within,
            // so we also check the parent element its role as work around.
            return element.parentElement();
          }
        } catch {
          // If element is stale or not attached yet, ignore
        }
      }
    });
  }

  getByLabel(label: string): PageObjectElement {
    return new WebDriverPageObjectElement(this.#browser, async () => {
      // WebdriverIO doesn't support combining both text and role,
      // so instead we select all text matches and then check for
      // the matching role with the `getComputedRole` method.
      const elements = await (await this.#getElement()).$$(`aria/${label}`);
      for (const element of elements) {
        try {
          if (
            [
              "textbox",
              "searchbox",
              "spinbutton",
              "slider",
              "checkbox",
              "radio",
            ].includes(await element.getComputedRole())
          ) {
            return element;
          }
        } catch {
          // If element is stale or not attached yet, ignore
        }
      }
    });
  }

  getByText(text: string): PageObjectElement {
    return new WebDriverPageObjectElement(this.#browser, async () =>
      (await this.#getElement()).$(`aria/${text}`),
    );
  }

  async waitFor(): Promise<void> {
    await this.#getElement();
  }

  async isPresent(): Promise<boolean> {
    const element = await this.#selectElement();
    return element?.isExisting() ?? false;
  }

  async isDisabled(): Promise<boolean> {
    const element = await this.#getElement();
    return !(await element.isEnabled());
  }

  async click(): Promise<void> {
    const element = await this.#getElement();
    return element.click();
  }

  async input(value: string | number): Promise<void> {
    const element = await this.#getElement();
    return element.setValue(value);
  }

  async isChecked(): Promise<boolean> {
    const element = await this.#getElement();
    return element.isSelected();
  }
}
