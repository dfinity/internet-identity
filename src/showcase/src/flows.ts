import { _SERVICE } from "$generated/internet_identity_types";
import { authenticateBoxFlow } from "$src/components/authenticateBox";
import { withLoader } from "$src/components/loader";
import { toast } from "$src/components/toast";
import { registerFlow, RegisterFlowOpts } from "$src/flows/register";
import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { MultiWebAuthnIdentity } from "$src/utils/multiWebAuthnIdentity";
import { ActorSubclass } from "@dfinity/agent";
import { DelegationIdentity } from "@dfinity/identity";
import { html, render, TemplateResult } from "lit-html";
import { dummyChallenge } from "./constants";
import { i18n } from "./i18n";
import { manageTemplates } from "./templates";

const registerSuccessToastTemplate = (result: unknown) => html`
  Identity successfully created!<br />
  <p class="l-stack">
    <strong class="t-strong">${prettyResult(result)}</strong>
  </p>
  <button
    class="l-stack c-button c-button--secondary"
    @click=${() => window.location.reload()}
  >
    reload
  </button>
`;

export const flowsPage = () => {
  document.title = "Flows";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};

const mockDelegationIdentity = {} as DelegationIdentity;
const mockActor = {
  identity_info: () => {
    return {
      Ok: {
        authn_methods: [],
        metadata: [],
        authn_method_registration: [],
      },
    };
  },
} as unknown as ActorSubclass<_SERVICE>;

class MockAuthenticatedConnection extends AuthenticatedConnection {
  constructor() {
    super(
      "12345",
      MultiWebAuthnIdentity.fromCredentials([]),
      mockDelegationIdentity,
      BigInt(12345),
      mockActor
    );
  }
  setShownRecoveryWarningPage = async (): Promise<void> => {
    // Do nothing
  };
}

const mockConnection = new MockAuthenticatedConnection();

const registerFlowOpts: RegisterFlowOpts = {
  createChallenge: async () => {
    await new Promise((resolve) => setTimeout(resolve, 2000));
    return dummyChallenge;
  },
  register: async ({ challengeResult: { chars } }) => {
    await new Promise((resolve) => setTimeout(resolve, 2000));

    if (chars !== "8wJ6Q") {
      return { kind: "badChallenge" };
    }
    return {
      kind: "loginSuccess",
      userNumber: BigInt(12356),
      connection: mockConnection,
    };
  },
  registrationAllowed: true,
  storePinIdentity: () => {
    toast.info("PIN identity was stored");
    return Promise.resolve();
  },
  pinAllowed: () => Promise.resolve(false),
  uaParser: Promise.resolve(undefined),
} as const;

export const iiFlows: Record<string, () => void> = {
  loginManage: async () => {
    const result = await authenticateBoxFlow<"identity">({
      i18n,
      templates: manageTemplates,
      addDevice: () => {
        toast.info(html`Added device`);
        return Promise.resolve({
          tag: "deviceAdded",
          alias: "My Device",
        });
      },
      loginPasskey: async () => {
        await new Promise((resolve) => setTimeout(resolve, 2000));
        toast.info(html`Logged in`);
        return Promise.resolve({
          kind: "loginSuccess",
          userNumber: BigInt(1234),
          connection: mockConnection,
        });
      },
      allowPinAuthentication: true,
      loginPinIdentityMaterial: async ({ pin }) => {
        toast.info(html`Valid PIN is '123456'`);
        await withLoader(
          () => new Promise((resolve) => setTimeout(resolve, 2000))
        );
        if (pin !== "123456") {
          return Promise.resolve({ kind: "badPin" });
        }
        toast.info(html`Logged in`);
        return Promise.resolve({
          kind: "loginSuccess",
          userNumber: BigInt(1234),
          connection: mockConnection,
        });
      },
      recover: () => {
        toast.info(html`Recovered`);
        return Promise.resolve({
          kind: "loginSuccess",
          userNumber: BigInt(1234),
          connection: mockConnection,
        });
      },
      retrievePinIdentityMaterial: ({ userNumber }) => {
        toast.info(
          html`Looking up identity for ${userNumber} (pretending only 10000 has
          pin identity)`
        );

        if (userNumber !== BigInt(10000)) {
          return Promise.resolve(undefined);
        }

        return Promise.resolve("identity");
      },
      verifyPinValidity: async () => {
        await new Promise((resolve) => setTimeout(resolve, 2000));
        return "valid";
      },
      registerFlowOpts,
    });
    toast.success(html`
      Authentication complete!<br />
      <p class="l-stack">
        <strong class="t-strong">${prettyResult(result)}</strong>
      </p>
      <button
        class="l-stack c-button c-button--secondary"
        @click=${() => window.location.reload()}
      >
        reload
      </button>
    `);
  },
  register: async () => {
    const result = await registerFlow(registerFlowOpts);
    toast.success(registerSuccessToastTemplate(result));
  },
  registerWithPin: async () => {
    const result = await registerFlow({
      ...registerFlowOpts,
      pinAllowed: () => Promise.resolve(true),
    });
    toast.success(registerSuccessToastTemplate(result));
  },
};

const pageContent: TemplateResult = html`
  <div class="l-wrap">
    <div class="l-container">
      <div class="c-card c-card--background">
        <h1 class="t-title t-title--main">Flows</h1>
        <div class="l-stack">
          ${Object.entries(iiFlows).map(([flowName, _]) => {
            // '/' or '/internet-identity/'
            const baseUrl = import.meta.env.BASE_URL ?? "/";
            // '/myFlow' or '/internet-identity/myFlow'
            const flowLink = baseUrl + "flows/" + flowName;
            return html`<aside>
              <a data-page-name=${flowName} href=${flowLink}>
                <h2>${flowName}</h2>
              </a>
            </aside>`;
          })}
        </div>
      </div>
    </div>
  </div>
`;

const prettyResult = (obj: unknown) => {
  if (typeof obj === "string") {
    return obj;
  }

  if (typeof obj === "object" && obj !== null) {
    return html`<ul>
      ${Object.entries(obj).map(
        ([k, v]) => html`<li><strong class="t-strong">${k}: ${v}</strong></li>`
      )}
    </ul>`;
  }

  return JSON.stringify(obj);
};
