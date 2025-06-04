/**
 * A platform-agnostic interface for page objects to interact with elements.
 * Platforms might include Jest and Playwright.
 *
 * Element locator methods are based on accessible selectors in Playwright,
 * this makes sure that E2E tests cover accessibility and are less likely
 * to break over time when changes are made to page structure and attributes.
 */
export interface PageObjectElement {
  /**
   * Reflects how users and assistive technology perceive the page,
   * for example whether some element is a button or a checkbox.
   */
  getByRole(role: string, { name }: { name: string }): PageObjectElement;

  /**
   * Most form controls usually have dedicated labels that could be
   * conveniently used to interact with the form.
   *
   * In this case, you can locate the control by its associated label.
   */
  getByLabel(label: string): PageObjectElement;

  /**
   * Find an element by the text it contains.
   *
   * Use `getByRole` and `getByLabel` instead if the element is interactive.
   */
  getByText(text: string): PageObjectElement;

  waitFor(): Promise<void>;

  isPresent(): Promise<boolean>;

  isDisabled(): Promise<boolean>;

  click(): Promise<void>;

  input(value: string): Promise<void>;

  isChecked(): Promise<boolean>;
}
