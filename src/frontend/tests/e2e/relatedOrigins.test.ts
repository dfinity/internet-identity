import {
  addVirtualAuthenticator,
  createActor,
  mimickPasskeyExtension,
  removeVirtualAuthenticator,
  runInBrowser,
} from "./util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { FLOWS } from "./flows";
import { MainView } from "./views";
import { DEVICE_NAME1, II_URL } from "./constants";

const EXPECTED_RELATED_ORIGINS = [
  "https://identity.internetcomputer.org",
  "https://identity.ic0.app",
];

test("Sign in on related origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Get related origins
    await browser.url(II_URL);
    const actor = await createActor(browser);
    const config = await actor.config();
    const relatedOrigins = config.related_origins[0] ?? [II_URL];

    // Related origins should be configured to the expected values
    expect(relatedOrigins).toStrictEqual(EXPECTED_RELATED_ORIGINS);

    // Register on main (current) origin
    await addVirtualAuthenticator(browser);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();

    // Sign in on each related origin
    for (const relatedOrigin of relatedOrigins) {
      // Sign out and navigate to different origin
      await mainView.logout();
      await browser.url(relatedOrigin);

      await FLOWS.loginExistingAuthenticateView(
        userNumber,
        DEVICE_NAME1,
        browser,
      );
    }
  });
}, 300_000);

test("Add devices on related origins with same origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Get related origins
    await browser.url(II_URL);
    const actor = await createActor(browser);
    const config = await actor.config();
    const relatedOrigins = config.related_origins[0] ?? [II_URL];

    // Related origins should be configured to the expected values
    expect(relatedOrigins).toStrictEqual(EXPECTED_RELATED_ORIGINS);

    // Register on main (current) origin
    const signInDevice = await addVirtualAuthenticator(browser);
    await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await removeVirtualAuthenticator(browser, signInDevice);

    // Since we're going to need to sign in to add a new device on each origin,
    // let's use the recovery as workaround to the single device limitation.
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);

    // Register device on each related origin
    for (const relatedOrigin of relatedOrigins) {
      // Sign out and navigate to different origin
      await mainView.logout();
      const additionalDevice = await addVirtualAuthenticator(browser);
      await browser.url(relatedOrigin);

      // Sign in using seed phrase and register device
      await FLOWS.recoverUsingSeedPhrase(browser, seedPhrase);
      await FLOWS.addFidoDevice(browser);
      await mainView.waitForDisplay();
      await removeVirtualAuthenticator(browser, additionalDevice);
    }

    // Check if all devices are registered with same origin
    await mainView.waitForDeviceCount(DEVICE_NAME1, relatedOrigins.length + 1);
    await mainView.waitForDifferentOriginDevice(false);
  });
}, 300_000);

test("Add devices on related origins with different origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Get related origins
    await browser.url(II_URL);
    const actor = await createActor(browser);
    const config = await actor.config();
    const relatedOrigins = config.related_origins[0] ?? [II_URL];

    // Related origins should be configured to the expected values
    expect(relatedOrigins).toStrictEqual(EXPECTED_RELATED_ORIGINS);

    // Register on main (current) origin
    const signInDevice = await addVirtualAuthenticator(browser);
    await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await removeVirtualAuthenticator(browser, signInDevice);

    // Since we're going to need to sign in to add a new device on each origin,
    // let's use the recovery as workaround to the single device limitation.
    const seedPhrase = await FLOWS.addRecoveryMechanismSeedPhrase(browser);

    // Register device on each related origin
    for (const relatedOrigin of relatedOrigins) {
      // Sign out and navigate to different origin
      await mainView.logout();
      const additionalDevice = await addVirtualAuthenticator(browser);
      await browser.url(relatedOrigin);

      // Disable RoR by mimicking a passkey browser extension,
      // this enables adding devices in different origins.
      await mimickPasskeyExtension(browser);

      // Sign in using seed phrase and register device
      await FLOWS.recoverUsingSeedPhrase(browser, seedPhrase);
      await FLOWS.addFidoDevice(browser);
      await mainView.waitForDisplay();
      await removeVirtualAuthenticator(browser, additionalDevice);
    }

    // Check if any devices are registered with different origins
    await mainView.waitForDeviceCount(DEVICE_NAME1, relatedOrigins.length + 1);
    await mainView.waitForDifferentOriginDevice(true);
  });
}, 300_000);

test("Use recovery device on related origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Get related origins
    await browser.url(II_URL);
    const actor = await createActor(browser);
    const config = await actor.config();
    const relatedOrigins = config.related_origins[0] ?? [II_URL];

    // Related origins should be configured to the expected values
    expect(relatedOrigins).toStrictEqual(EXPECTED_RELATED_ORIGINS);

    // Register on main (current) origin
    const signInDevice = await addVirtualAuthenticator(browser);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await removeVirtualAuthenticator(browser, signInDevice);

    // Create recovery device
    await addVirtualAuthenticator(browser);
    await FLOWS.addRecoveryMechanismDevice(browser);

    // Recover on each related origin
    for (const relatedOrigin of relatedOrigins) {
      // Sign out and navigate to different origin
      await mainView.logout();
      await browser.url(relatedOrigin);

      await FLOWS.recoverUsingDevice(browser, userNumber);
    }
  });
}, 300_000);
