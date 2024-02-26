import {
  AddDeviceSuccessView,
  AddRemoteDeviceInstructionsView,
  AuthenticateView,
  MainView,
  PinAuthView,
  PinRegistrationView,
  RecoverView,
  RecoveryMethodSelectorView,
  RegisterView,
  WelcomeView,
} from "./views";

export const FLOWS = {
  register: async function (browser: WebdriverIO.Browser): Promise<string> {
    const registerView = new RegisterView(browser);
    await registerView.waitForDisplay();
    await registerView.create();
    await registerView.waitForRegisterConfirm();
    await registerView.confirmRegisterConfirm();
    await registerView.waitForIdentity();
    const userNumber = await registerView.registerGetIdentity();
    await registerView.registerConfirmIdentity();
    return userNumber;
  },
  registerNewIdentityWelcomeView: async (
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    return await FLOWS.register(browser);
  },
  registerNewIdentityAuthenticateView: async (
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.register();
    return await FLOWS.register(browser);
  },
  registerPin: async function (
    browser: WebdriverIO.Browser,
    pin: string
  ): Promise<string> {
    const registerView = new RegisterView(browser);
    await registerView.waitForDisplay();
    await registerView.createPin();
    const pinRegistrationView = new PinRegistrationView(browser);
    await pinRegistrationView.waitForPinInfo();
    await pinRegistrationView.pinInfoContinue();
    await pinRegistrationView.waitForSetPin();
    await pinRegistrationView.setPin(pin);
    await pinRegistrationView.waitForConfirmPin();
    await pinRegistrationView.confirmPin(pin);
    await registerView.waitForRegisterConfirm();
    await registerView.confirmRegisterConfirm();
    await registerView.waitForIdentity();
    const userNumber = await registerView.registerGetIdentity();
    await registerView.registerConfirmIdentity();
    return userNumber;
  },
  registerPinWelcomeView: async (
    browser: WebdriverIO.Browser,
    pin: string
  ): Promise<string> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    return await FLOWS.registerPin(browser, pin);
  },
  registerPinNewIdentityAuthenticateView: async (
    pin: string,
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.register();
    return await FLOWS.registerPin(browser, pin);
  },
  loginWelcomeView: async (
    userNumber: string,
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<void> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.login(userNumber);
    // This flow assumes no recovery phrase, so we explicitly skip the recovery nag here
    await FLOWS.skipRecoveryNag(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(deviceName);
  },
  loginAuthenticateView: async (
    userNumber: string,
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<void> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    // This flow assumes no recovery phrase, so we explicitly skip the recovery nag here
    await FLOWS.skipRecoveryNag(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(deviceName);
  },

  // A PIN auth view that's allowed to fail
  loginPinAuthenticateView_: async (
    userNumber: string,
    pin: string,
    browser: WebdriverIO.Browser
  ): Promise<"ok" | "pin_disallowed"> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    const pinAuthView = new PinAuthView(browser);
    // Here we race two things:
    // * The PIN authn view
    // * Expecting an error because PIN is not allowed
    // If the PIN authn view is returned then carry forward;
    // otherwise return that PIN is not allowed
    const result = await Promise.race([
      pinAuthView.waitForDisplay().then(() => "pin_allowed" as const),
      browser
        .$('#errorContainer [data-error-code="pinNotAllowed"]')
        .waitForDisplayed()
        .then(() => "pin_disallowed" as const),
    ]);
    if (result === "pin_disallowed") {
      return "pin_disallowed";
    }
    result satisfies "pin_allowed";

    await pinAuthView.enterPin(pin);
    // This flow assumes no recovery phrase, so we explicitly skip the recovery nag here
    await FLOWS.skipRecoveryNag(browser);
    return "ok";
  },
  loginPinAuthenticateView: async (
    userNumber: string,
    pin: string,
    browser: WebdriverIO.Browser
  ): Promise<void> => {
    const result = await FLOWS.loginPinAuthenticateView_(
      userNumber,
      pin,
      browser
    );
    if (result === "pin_disallowed") {
      throw new Error(
        "Expected successful pin authentication, got PIN disallowed"
      );
    }

    result satisfies "ok";
  },
  loginPinWelcomeView: async (
    userNumber: string,
    pin: string,
    browser: WebdriverIO.Browser
  ): Promise<void> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.login(userNumber);
    const pinAuthView = new PinAuthView(browser);
    await pinAuthView.waitForDisplay();
    await pinAuthView.enterPin(pin);
    // This flow assumes no recovery phrase, so we explicitly skip the recovery nag here
    await FLOWS.skipRecoveryNag(browser);
  },
  addRecoveryMechanismSeedPhrase: async (
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await mainView.addRecovery();

    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForSeedPhrase();
    const seedPhrase = await recoveryMethodSelectorView.getSeedPhrase();
    await recoveryMethodSelectorView.acknowledgeCheckbox();
    await recoveryMethodSelectorView.seedPhraseContinue();
    await recoveryMethodSelectorView.seedPhraseFill();

    // Wait for the main view to be displayed again to ensure that the recovery
    // mechanism was added successfully.
    await mainView.waitForDisplay();

    return seedPhrase;
  },
  readSeedPhrase: async (browser: WebdriverIO.Browser): Promise<string> => {
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForSeedPhrase();
    const seedPhrase = await recoveryMethodSelectorView.getSeedPhrase();
    await recoveryMethodSelectorView.acknowledgeCheckbox();
    await recoveryMethodSelectorView.seedPhraseContinue();
    await recoveryMethodSelectorView.seedPhraseFill();

    return seedPhrase;
  },
  addFidoDevice: async (browser: WebdriverIO.Browser): Promise<void> => {
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await mainView.addAdditionalDevice();
    const addRemoteDeviceInstructionsView = new AddRemoteDeviceInstructionsView(
      browser
    );
    await addRemoteDeviceInstructionsView.addFIDODevice();

    await browser.pause(10_000);

    // success page
    const addDeviceSuccessView = new AddDeviceSuccessView(browser);
    await addDeviceSuccessView.waitForDisplay();
    await addDeviceSuccessView.continue();
  },
  skipRecoveryNag: async (browser: WebdriverIO.Browser): Promise<void> => {
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skipRecovery();
  },
  recoverUsingSeedPhrase: async (
    browser: WebdriverIO.Browser,
    recoveryPhrase: string
  ): Promise<void> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.recover();
    const recoveryView = new RecoverView(browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(recoveryPhrase);
    await recoveryView.enterSeedPhraseContinue();
    await recoveryView.skipDeviceEnrollment();
  },
};
