import { CAPTCHA_ENABLED } from "./constants";
import {
  AddDeviceSuccessView,
  AddRemoteDeviceInstructionsView,
  AuthenticateView,
  MainView,
  PinAuthView,
  PinRegistrationView,
  PromptDeviceAliasView,
  PromptUserNumberView,
  RecoverSeedPhraseView,
  RecoveryMethodSelectorView,
  RegisterView,
  WelcomeView,
} from "./views";

export const FLOWS = {
  register: async function (browser: WebdriverIO.Browser): Promise<string> {
    const registerView = new RegisterView(browser);
    if (CAPTCHA_ENABLED) {
      await registerView.waitForRegisterConfirm();
      await registerView.confirmRegisterConfirm();
    }
    await registerView.waitForIdentity();
    const userNumber = await registerView.registerGetIdentity();
    await registerView.registerConfirmIdentity();
    return userNumber;
  },
  registerNewIdentityWelcomeView: async (
    browser: WebdriverIO.Browser,
  ): Promise<string> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    return await FLOWS.register(browser);
  },
  registerNewIdentityAuthenticateView: async (
    browser: WebdriverIO.Browser,
  ): Promise<string> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.register();
    return await FLOWS.register(browser);
  },
  registerPin: async function (
    browser: WebdriverIO.Browser,
    pin: string,
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
    if (CAPTCHA_ENABLED) {
      await registerView.waitForRegisterConfirm();
      await registerView.confirmRegisterConfirm();
    }
    await registerView.waitForIdentity();
    const userNumber = await registerView.registerGetIdentity();
    await registerView.registerConfirmIdentity();
    return userNumber;
  },
  registerPinWelcomeView: async (
    browser: WebdriverIO.Browser,
    pin: string,
  ): Promise<string> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    return await FLOWS.registerPin(browser, pin);
  },
  registerPinNewIdentityAuthenticateView: async (
    pin: string,
    browser: WebdriverIO.Browser,
  ): Promise<string> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.register();
    return await FLOWS.registerPin(browser, pin);
  },
  loginWelcomeView: async (
    userNumber: string,
    browser: WebdriverIO.Browser,
  ): Promise<void> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.login(userNumber);
  },
  loginAuthenticateView: async (
    userNumber: string,
    deviceName: string,
    browser: WebdriverIO.Browser,
  ): Promise<void> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(deviceName);
  },
  loginExistingAuthenticateView: async (
    userNumber: string,
    deviceName: string,
    browser: WebdriverIO.Browser,
  ): Promise<void> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.continueWithAnchor(userNumber);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(deviceName);
  },
  loginPinAuthenticateView: async (
    userNumber: string,
    pin: string,
    browser: WebdriverIO.Browser,
  ): Promise<void> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    const pinAuthView = new PinAuthView(browser);
    await pinAuthView.waitForDisplay();
    await pinAuthView.enterPin(pin);
  },
  loginPinWelcomeView: async (
    userNumber: string,
    pin: string,
    browser: WebdriverIO.Browser,
  ): Promise<void> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.login(userNumber);
    const pinAuthView = new PinAuthView(browser);
    await pinAuthView.waitForDisplay();
    await pinAuthView.enterPin(pin);
  },
  addRecoveryMechanismSeedPhrase: async (
    browser: WebdriverIO.Browser,
  ): Promise<string> => {
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await mainView.addRecoverySeedPhrase();

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
  addRecoveryMechanismDevice: async (
    browser: WebdriverIO.Browser,
  ): Promise<void> => {
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();
    await mainView.addRecoveryDevice();
    await mainView.waitForDisplay();
    await browser.pause(10_000);

    // Wait for the main view to be displayed again to ensure that the recovery
    // mechanism was added successfully.
    await mainView.waitForDisplay();
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
      browser,
    );
    await addRemoteDeviceInstructionsView.addFIDODevice();

    await browser.pause(10_000);

    // success page
    const addDeviceSuccessView = new AddDeviceSuccessView(browser);
    await addDeviceSuccessView.waitForDisplay();
    await addDeviceSuccessView.continue();
  },
  recoverUsingSeedPhrase: async (
    browser: WebdriverIO.Browser,
    recoveryPhrase: string,
  ): Promise<void> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.recoverSeedPhrase();
    const recoveryView = new RecoverSeedPhraseView(browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(recoveryPhrase);
    await recoveryView.enterSeedPhraseContinue();
    await recoveryView.skipDeviceEnrollment();
  },
  recoverUsingDevice: async (
    browser: WebdriverIO.Browser,
    userNumber: string,
  ): Promise<void> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.recoverDevice();
    const userNumberView = new PromptUserNumberView(browser);
    await userNumberView.waitForUserNumberDisplay();
    await userNumberView.enterUserNumber(userNumber);
    await userNumberView.enterUserNumberContinue();
    const deviceAliasView = new PromptDeviceAliasView(browser);
    await deviceAliasView.waitForDeviceAliasDisplay();
    await deviceAliasView.skipDeviceAlias();
  },
};
