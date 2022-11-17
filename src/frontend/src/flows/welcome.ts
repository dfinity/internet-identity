import { render, html, TemplateResult } from "lit-html";
import { registerIfAllowed } from "../utils/registerAllowedCheck";
import { setUserNumber } from "../utils/userNumber";
import { unreachable } from "../utils/utils";
import { Connection } from "../utils/iiConnection";
import { LoginFlowResult, LoginData } from "../flows/login/flowResult";
import { displayError } from "../components/displayError";
import { footer } from "../components/footer";
import { icLogo } from "../components/icons";
import { auth } from "../flows/login";

/** Properties of the "Welcome" screen, which welcomes new users. */
type WelcomeProps = {
  register: () => void;
  signin: () => void;
};

const learnMore = html`
  <p class="t-paragraph t-centered l-stack">
    <a
      class="t-link"
      href="https://medium.com/dfinity/internet-identity-the-end-of-usernames-and-passwords-ff45e4861bf7"
      target="_blank"
      rel="noopener noreferrer"
      >Learn More</a
    >
  </p>
`;

const pageContent = ({
  register,
  signin,
}: WelcomeProps): TemplateResult => html` <section
    class="l-container c-card c-card--highlight"
    aria-label="Authentication"
  >
    <div class="c-logo">${icLogo}</div>
    <p class="t-paragraph t-centered">Use anchors to authenticate to dapps<br/>on the Internet Computer</p>
    <article class="l-stack">
      <button
        type="button"
        @click=${register}
        id="registerButton"
        class="c-button"
      >
        Create an Anchor
      </button>
      <button
        type="button"
        @click=${signin}
        id="loginButton"
        class="c-button c-button--secondary"
      >
        Manage Existing
      </button>
    </article>

    ${learnMore}
  </section>
  ${footer}`;

export const welcomePage = (props: WelcomeProps): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(props), container);
};

export const welcome = (connection: Connection): Promise<LoginData> => {
  const retryOnError = async (result: LoginFlowResult): Promise<LoginData> => {
    switch (result.tag) {
      case "err":
        await displayError({
          title: result.title,
          message: result.message,
          detail: result.detail !== "" ? result.detail : undefined,
          primaryButton: "Try again",
        });
        return welcome(connection);
      case "ok":
        return result;
      case "canceled":
        return welcome(connection);
      default:
        unreachable(result);
        break;
    }
  };

  return new Promise<LoginData>((resolve) => {
    welcomePage({
      register: async () => {
        const loginData = await registerIfAllowed(connection).then(
          retryOnError
        );
        setUserNumber(loginData.userNumber);
        resolve(loginData);
      },
      signin: async () => {
        const loginData = await auth(connection);
        resolve(loginData);
      },
    });
  });
};
