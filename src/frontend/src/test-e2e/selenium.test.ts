import {
  AboutView,
  AddDeviceAliasView,
  AddDeviceView,
  AddIdentityAnchorView,
  AuthorizeAppView,
  CompatabilityNoticeView,
  DemoAppView,
  FAQView,
  MainView,
  RecoverView,
  RecoveryMethodSelectorView,
  RegisterView,
  SingleDeviceWarningView,
  WelcomeBackView,
  WelcomeView,
} from "./views";
import { FLOWS } from "./flows";
import {
  Screenshots,
  addVirtualAuthenticator,
  removeVirtualAuthenticator,
  runInBrowser,
  runInNestedBrowser,
  switchToPopup,
  waitForFonts,
  waitToClose,
  setupSeleniumServer,
  RunConfiguration,
} from "./util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import canister_ids1 from "../../../../.dfx/local/canister_ids.json";
//import canister_ids2 from "../../../../demos/whoami/.dfx/local/canister_ids.json";

const IDENTITY_CANISTER = canister_ids1.internet_identity.local;
//const WHOAMI_CANISTER = canister_ids2.whoami.local;

const REPLICA_URL = "http://localhost:8000";
const II_URL = `http://localhost:8000/?canisterId=${IDENTITY_CANISTER}`;
const FAQ_URL = `http://localhost:8000/faq?canisterId=${IDENTITY_CANISTER}`;
const ABOUT_URL = `http://localhost:8000/about?canisterId=${IDENTITY_CANISTER}`;
const DEMO_APP_URL = "http://localhost:8080/";

const DEVICE_NAME1 = "Virtual WebAuthn device";
const DEVICE_NAME2 = "Other WebAuthn device";

setupSeleniumServer();

test("Register new identity and login with it", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentity(DEVICE_NAME1, browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.logout();
    await FLOWS.login(userNumber, DEVICE_NAME1, browser);
  });
}, 300_000);
