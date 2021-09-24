import { AuthorizeAppView, DemoAppView, MainView, RecoverView, WelcomeView, } from "./views";
import { runInBrowser, waitToClose, } from "./util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import canister_ids1 from "../../../../.dfx/local/canister_ids.json";
import canister_ids2 from "../../../../demos/whoami/.dfx/local/canister_ids.json";
import { fromMnemonicWithoutValidation } from "../crypto/ed25519";
import { Principal } from "@dfinity/principal";
import { hasOwnProperty } from "../utils/utils";
import getProofOfWork from "../crypto/pow";
import { DeviceData } from "../../generated/internet_identity_types";
import { IC_DERIVATION_PATH, IIConnection, requestFEDelegation, } from "../utils/iiConnection";
import { generate } from "../crypto/mnemonic";

const IDENTITY_CANISTER = canister_ids1.internet_identity.local;
const WHOAMI_CANISTER = canister_ids2.whoami.local;

const REPLICA_URL = "http://localhost:8000";
const II_URL = `http://localhost:8080`;
const FAQ_URL = `http://localhost:8000/faq?canisterId=${IDENTITY_CANISTER}`;
const ABOUT_URL = `http://localhost:8000/about?canisterId=${IDENTITY_CANISTER}`;
const DEMO_APP_URL = "http://localhost:8081/";

const DEVICE_NAME1 = "Virtual WebAuthn device";
const DEVICE_NAME2 = "Other WebAuthn device";

async function setupTestUser() {
  // create a new identity
  const testPhrase =
    "script strategy fat bonus minimum elegant art hire vital palace combine vague proud exercise arrow copy media aim sleep soul energy crane amazing rely";

  const testIdentity = await fromMnemonicWithoutValidation(testPhrase);

  const delegationIdentity = await requestFEDelegation(testIdentity);
  const actor = await IIConnection.createActor(delegationIdentity);

  const testDevice: DeviceData = {
    alias: "testDevice",
    pubkey: Array.from(testIdentity.getPublicKey().toDer()),
    key_type: { unknown: null },
    purpose: { authentication: null },
    credential_id: [],
  };

  const now_in_ns = BigInt(Date.now()) * BigInt(1000000);
  const pow = getProofOfWork(now_in_ns, Principal.fromText(IDENTITY_CANISTER));

  const registerResponse = await actor.register(testDevice, pow);

  let anchor;
  if (hasOwnProperty(registerResponse, "registered")) {
    anchor = registerResponse.registered.user_number;
  } else {
    throw new Error("Failed to create test user");
  }

  // add a recovery identity
  const name = "Recovery phrase";
  const seedPhrase = generate().trim();
  const recoverIdentity = await fromMnemonicWithoutValidation(
    seedPhrase,
    IC_DERIVATION_PATH
  );
  await actor.add(anchor, {
    alias: name,
    pubkey: Array.from(recoverIdentity.getPublicKey().toDer()),
    credential_id: [],
    key_type: { seed_phrase: null },
    purpose: { recovery: null },
  });

  return {
    anchor: registerResponse.registered.user_number,
    seedPhrase: seedPhrase,
  };
}

test("Recover access for test user", async () => {
  const { anchor, seedPhrase } = await setupTestUser();

  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const welcomeView = new WelcomeView(browser);
    await welcomeView.recover();
    const recoveryView = new RecoverView(browser);
    await recoveryView.waitForDisplay();
    await recoveryView.enterIdentityAnchor(anchor.toString());
    await recoveryView.continue();
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(seedPhrase);
    await recoveryView.enterSeedPhraseContinue();
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay("testDevice");
  });
}, 300_000);

test("Authenticate using mock login", async () => {
  const { anchor } = await setupTestUser();

  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(DEMO_APP_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.signin();

    const handles = await browser.getWindowHandles();
    expect(handles.length).toBe(2);
    await browser.switchToWindow(handles[1]);

    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.typeUserNumber(anchor.toString());
    await welcomeView.login();

    const authorizeAppView = new AuthorizeAppView(browser);
    await authorizeAppView.waitForDisplay();
    await authorizeAppView.confirm();
    await waitToClose(browser);
    await demoAppView.waitForDisplay();
    const principal = await demoAppView.getPrincipal();
    expect(principal).not.toBe("2vxsx-fae");

    // default value
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (30 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);