/** A showcase of common CSS patterns that can be reuses all all over the app */
import "./styles/main.css";
import { html, render } from "lit-html";
import { icLogo, settingsIcon, dropdownIcon } from "./components/icons";
import { warnBox } from "./components/warnBox";
import { irregularity } from "./components/irregularity";

const removeToast = (toast: HTMLElement): void => {
  toast.addEventListener("animationend", () => {
    toast.remove();
  });
  toast.classList.add("c-toast--closing");
};

const createToast = (): void => {
  const message = html`
    This is a toast message. <a href="#">This is a link</a>. Message Nr.
    ${Math.floor(Math.random() * 1000)}
  `;
  const toast = document.createElement("div");
  toast.classList.add("c-toast");
  render(
    irregularity({
      message,
      closeFn: () => removeToast(toast),
    }),
    toast
  );
  document.querySelector("[data-toasts]")?.appendChild(toast);
};

export const styleguide = html`
  <style>
    .styleguide {
      margin: 10rem auto;
      max-width: 60rem;
      padding: 0 2rem;
    }
    .styleguide code {
      background-color: #202124;
      color: #fff;
      padding: 0.2em;
    }

    .demo {
      z-index: 0;
      position: relative;

      min-height: 15rem;

      margin: 2rem 0;
      border: rgba(255, 255, 255, 0) 4px solid;
      padding: 1em;
      background-color: #f7f7f7;
      background-image: linear-gradient(
          45deg,
          rgba(0, 0, 0, 0.05) 25%,
          transparent 25%,
          transparent 75%,
          rgba(0, 0, 0, 0.05) 75%,
          rgba(0, 0, 0, 0.05)
        ),
        linear-gradient(
          45deg,
          rgba(0, 0, 0, 0.05) 25%,
          transparent 25%,
          transparent 75%,
          rgba(0, 0, 0, 0.05) 75%,
          rgba(0, 0, 0, 0.05)
        );
      background-size: 3rem 3rem;
      background-position: 0 0, 1.5rem 1.5rem;
      border-radius: 0.5em;

      box-shadow: inset 0 0 1rem rgba(0, 0, 0, 0.5);
    }

    .demo-section {
      background: #fff;
      padding: 2.5rem;
      border-radius: 0.5em;
      box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.1);
    }
  </style>
  <section class="styleguide">
    <h1 class="t-title t-title--main">Design Patterns</h1>
    <article class="l-stack c-card c-card--highlight">
      <h1 class="t-title t-title--main">Typography</h1>
      <p class="t-lead">
        The font used all over the app is called "Montserrat" and is loaded from
        <a href="https://fonts.google.com/specimen/Montserrat">Google Fonts</a>.
        It is a sans-serif font that is easy to read.
      </p>
      <p class="t-paragrapgh">
        The base font size is dependent on the browser's window size. But 1rem
        is our base unit. To make it easier to work with it, we consider 1rem
        beeing the equivalent of ~10px.
      </p>
      <p class="t-paragrapgh">
        Since we use a CSS reset almost no html elements come with a default
        styling. To help style text element we have a bunch of utility classes
        that can be used.
      </p>
      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Tiltes</h2>
        <section class="demo" aria-label="Titles Demo">
          <h1 class="t-title t-title--main">Large Title</h1>
          <h2 class="t-title">Default regular title</h2>
        </section>
        <p class="t-lead">
          The <code>.t-title</code> class can be used to style titles. If you
          want to make it a main title, add the
          <code>.t-title--main</code> modifier class.
        </p>
      </aside>
      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Paragraphs</h2>
        <section class="demo" aria-label="Paragraphs Demo">
          <p class="t-lead">This is a lead paragraph</p>
          <p class="t-paragraph">This is a regular paragraph</p>
        </section>
        <p class="t-lead">
          There are two types of text paragraphs. The <code>.t-lead</code> class
          can be used to style lead paragraphs that follow a title. The
          <code>.t-paragraph</code> class is a more generic class that can be
          used to style any other paragraph.
        </p>
      </aside>
      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Various text helpers</h2>
        <section class="demo" aria-label="Verious elements Demo">
          <p class="t-paragraph">
            <a href="#">This is an actual "a" tag</a><br />
            <button class="t-link">
              This is a button element that looks like a link</button
            ><br />
            <button class="t-link t-link--discreet">
              This is a link that is not underlined</button
            ><br />
            <a href="#" class="t-link"
              ><i class="t-link__icon">+</i>Link with icon</a
            ><br />
          </p>
        </section>
        <p class="t-lead">
          There are a bunch of other text helpers that can be used to style text
          within a paragraph or anywhere else. One of the rare elements that has
          a default style is a <a href="#">link</a>, but sometimes you might
          want an other element to look like a link. In that case you can use
          the <code>.t-link</code> class. In some rare cases we want links to
          look less prominent (by removing the underline) you can use the
          <code>.t-link--discreet</code> class.
        </p>
      </aside>
    </article>

    <article class="l-stack  c-card c-card--highlight">
      <h1 class="t-title t-title--main">Layout</h1>
      <p class="t-lead">
        We use a bunch of layout classes to style the
        <code>.c-card__icon</code> of the app. They just define where elements
        are placed.
      </p>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Sections</h2>
        <p class="t-lead">
          A section, much like a text-paragraph, is a visually distinct area of
          the app. In our case it just sets a space to the top. It can be used
          with the <code>.l-stack</code> class. Sometimes we want there to be
          more spaces between sections. In that case we can use the
          <code>.l-stack--spacious</code> modifier class.
        </p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Divider</h2>
        <section class="demo" aria-label="Paragraphs Demo">
          <hr class="l-divider" />
          <div class="l-divider l-divider--text" aria-label="Other Options">
            <span class="l-divider__label" aria-hidden="true">Or</span>
          </div>
        </section>
        <p class="t-lead">
          A divider is a horizontal line that can be used to separate sections.
          It can be used with the <code>.l-divider</code> class. Typically it is
          used on a <code>&lt;hr&gt;</code> element. But sometimes we would like
          to add some text to it. In that case we can use the
          <code>.l-divider--text</code> modifier class.
        </p>
      </aside>
    </article>

    <article class="l-stack c-card c-card--highlight">
      <h1 class="t-title t-title--main">Components</h1>
      <p class="t-lead">
        Components are the building blocks of the app. They consist of visual
        patterns that are used all over the app.
      </p>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Cards</h2>
        <section class="demo" aria-label="Cards Demo">
          <div class="c-card l-stack">
            <h2 class="t-title">Default card</h2>
          </div>
          <div class="c-card c-card--narrow l-stack">
            <h2 class="t-title">Narrow Card</h2>
          </div>
          ${warnBox({
            title: "Warning Card with Icon",
            message: "This is a card with a warning icon",
            additionalClasses: ["l-stack"],
          })}
          ${warnBox({
            title: "Actionable warning",
            message:
              "Sometimes we want to give the user the option to do something about the warning",
            additionalClasses: ["l-stack"],
            slot: html`<div class="c-button-group">
              <button class="c-button c-button--primary">Do something!</button>
              <button class="c-button c-button--secondary">Ignore</button>
            </div>`,
          })}
        </section>
        <p class="t-lead">
          The card component is used to group content. It can be used with the
          <code>.c-card</code> class. It comes with a few modifiers that can be
          used to style it differently.
        </p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title">Form Elements</h2>
        <section class="demo" aria-label="Form Elements Demo">
          <label class="c-input--anchor__wrap" aria-label="Identity Anchor">
            <input
              type="text"
              data-role="anchor-input"
              class="c-input c-input--vip c-input--spacious c-input--anchor"
              placeholder="Enter anchor"
            />
          </label>

          <input type="text" placeholder="Text Input" class="c-input" />
          <div class="c-input">DIV as c-input</div>
          <div class="c-input c-input--readonly">Readonly input</div>
          <div class="c-input c-input--vip">Important inputs</div>
          <input type="text" placeholder="Rounded Input" class="c-input" />
          <input
            type="text"
            placeholder="Spacious Input"
            class="c-input c-input--spacious"
          />
          <input
            type="text"
            placeholder="Centered Input"
            class="c-input c-input--centered"
          />
          <input
            type="text"
            placeholder="Errored Input"
            class="c-input has-error"
          />

          <button class="c-button">Primary Button</button>
          <button class="c-button c-button--secondary">Secondary Button</button>
          <button class="c-button c-button--disabled" disabled>
            Disabled Button
          </button>
          <button class="c-button c-button--warning">Warning Button</button>
        </section>
        <p class="t-lead">
          Form elements are used to collect information from the user. But we
          also use the classes to show important numbers and other information.
        </p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title">Lists</h2>
        <section class="demo" aria-label="List Elements Demo">
          <ul class="c-list">
            <li>Default list item I</li>
            <li>Default list item II</li>
            <li>Default list item III</li>
          </ul>

          <ul class="c-list c-list--bulleted l-stack">
            <li>Bulleted list item I</li>
            <li>Bulleted list item II</li>
            <li>Bulleted list item III</li>
          </ul>

          <ul class="c-list c-list--numbered l-stack">
            <li>Numbered list item I</li>
            <li>Numbered list item II</li>
            <li>Numbered list item III</li>
          </ul>
        </section>
        <p class="t-lead">
          The <code>.c-list</code> class is used to style list elements. It
          expects a <code>&lt;li&gt;</code> element as a direct child.
        </p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Details / Summary</h2>
        <section class="demo" aria-label="Details / Summary Demo">
          <details>
            <summary class="c-summary">
              <span class="c-summary__link t-link">Click me to open</span>
            </summary>
            <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed</p>
          </details>
        </section>
        <p class="t-lead">
          While the <code>&lt;details&gt;</code> element is not a component, it
          is expected to be used with the <code>.c-summary</code> class, since
          it relies on the <code>[open]</code> attribute.
        </p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Actionable Lists</h2>
        <section class="demo" aria-label="Details / Summary Demo">
          <ul class="c-action-list">
            <li class="c-action-list__item">
              <span class="c-action-list__label">Actionable List Item</span>
              <span class="c-action-list__action c-dropdown">
                <button
                  class="c-dropdown__trigger"
                  aria-expanded="false"
                  aria-controls="dropdown-i"
                >
                  ${settingsIcon}
                </button>
                <ul class="c-dropdown__menu" id="dropdown-i">
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Delete Anchor</button>
                  </li>
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Lock Anchor</button>
                  </li>
                </ul>
              </span>
            </li>
          </ul>

          <ul class="c-action-list">
            <li class="c-action-list__item">
              <span class="c-action-list__label">Actionable List Item</span>
              <span class="c-action-list__action c-dropdown">
                <button
                  class="c-dropdown__trigger"
                  aria-expanded="false"
                  aria-controls="dropdown-i"
                >
                  ${dropdownIcon}
                </button>
                <ul class="c-dropdown__menu" id="dropdown-i">
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Delete Anchor</button>
                  </li>
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Lock Anchor</button>
                  </li>
                </ul>
              </span>
            </li>
            <li class="c-action-list__item">
              <span class="c-action-list__label">Actionable List Item</span>
              <span class="c-action-list__action c-dropdown">
                <button
                  class="c-dropdown__trigger"
                  aria-expanded="false"
                  aria-controls="dropdown-i"
                >
                  ${dropdownIcon}
                </button>
                <ul class="c-dropdown__menu" id="dropdown-i">
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Delete Anchor</button>
                  </li>
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Lock Anchor</button>
                  </li>
                </ul>
              </span>
            </li>
            <li class="c-action-list__actions">
              <button class="c-button c-button--primary">Some action</button>
            </li>
          </ul>

          ${warnBox({
            title: "Devices",
            message:
              "We recommend that you have at least two devices (for example, your computer and your phone).",
            additionalClasses: ["l-stack"],
            slot: html` <ul class="c-action-list">
              <li class="c-action-list__item">
                <span class="c-action-list__label">Single Device</span>
                <button class="c-action-list__action">${settingsIcon}</button>
              </li>
              <li class="c-action-list__actions">
                <button class="c-button c-button--primary">Some action</button>
              </li>
            </ul>`,
          })}
        </section>
        <p class="t-lead"></p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title">Irregularities</h2>
        <p class="t-lead">
          Irregularities are messages that we show to the user when something
          goes wrong or something happens that we want to inform the user about.
          They can be used to show errors, warnings, or success messages.
        </p>
        <section class="demo" aria-label="Irregularity Elements Demo">
          ${irregularity({
            message:
              "This is an error message. It can be used to inform the user about something that went wrong.",
            errorType: "error",
          })}
          ${irregularity({
            message:
              "This is an error message. It can be used to inform the user about something that went wrong.",
            errorType: "error",
            closeFn: () => {
              console.log("close");
            },
          })}
        </section>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Logo</h2>
        <section class="demo" aria-label="Logo Demo">
          <div class="c-logo">${icLogo}</div>
        </section>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title">Toast</h2>
        <p class="t-lead">
          Toasts are messages of varying length and importance that appear at
          the bottom or the top of the screen. They typically use the
          Irregularity component.
        </p>
        <section class="demo" aria-label="Toast Elements Demo">
          <button
            class="c-button c-button--primary"
            @click="${() => createToast()}"
          >
            Show Toast
          </button>
          <div class="c-toasts">
            <div class="l-container" data-toasts></div>
          </div>
        </section>
      </aside>
    </article>
  </section>
`;
