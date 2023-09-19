import {
  AuthenticateView,
  MainView,
  PinAuthView,
  PinRegistrationView,
  RecoveryMethodSelectorView,
  RegisterView,
  WelcomeView,
} from "./views";

export const FLOWS = {
  register: async function (
    browser: WebdriverIO.Browser,
    deviceName: string
  ): Promise<string> {
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
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    return await FLOWS.register(browser, deviceName);
  },
  registerNewIdentityAuthenticateView: async (
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<string> => {
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.register();
    return await FLOWS.register(browser, deviceName);
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
  login: async (
    userNumber: string,
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<void> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.login();
    await welcomeView.typeUserNumber(userNumber);
    await browser.$("button[data-action='continue']").click();
    // NOTE: depending on the browser, we issue different warnings. On Safari,
    // the warning comes before the recovery method selector. Since we only
    // test on Chrome we always expect the recovery selector first.
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skipRecovery();
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(deviceName);
  },
  loginPin: async (
    userNumber: string,
    pin: string,
    deviceName: string,
    browser: WebdriverIO.Browser
  ): Promise<void> => {
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.login();
    await welcomeView.typeUserNumber(userNumber);
    await browser.$("button[data-action='continue']").click();
    const pinAuthView = new PinAuthView(browser);
    await pinAuthView.waitForDisplay();
    await pinAuthView.enterPin(pin);
    // NOTE: handle recovery nag because there is no recovery phrase
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(browser);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skipRecovery();
    const mainView = new MainView(browser);
    await mainView.waitForTempKeyDisplay(deviceName);
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
};
