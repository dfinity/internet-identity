import { ThenableWebDriver } from "selenium-webdriver";
import {
  MainView,
  RecoveryMethodSelectorView,
  RegisterView,
  SingleDeviceWarningView,
  WelcomeView,
} from "./views";

export const FLOWS = {
  registerNewIdentity: async (
    deviceName: string,
    driver: ThenableWebDriver
  ): Promise<string> => {
    const welcomeView = new WelcomeView(driver);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    const registerView = new RegisterView(driver);
    await registerView.waitForDisplay();
    await registerView.enterAlias(deviceName);
    await registerView.create();
    await registerView.waitForRegisterConfirm();
    await registerView.confirmRegisterConfirm();
    await registerView.waitForIdentity();
    const userNumber = await registerView.registerGetIdentity();
    await registerView.registerConfirmIdentity();
    const singleDeviceWarningView = new SingleDeviceWarningView(driver);
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.continue();
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(driver);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skipRecovery();
    return userNumber;
  },
  login: async (
    userNumber: string,
    deviceName: string,
    driver: ThenableWebDriver
  ): Promise<void> => {
    const welcomeView = new WelcomeView(driver);
    await welcomeView.waitForDisplay();
    await welcomeView.typeUserNumber(userNumber);
    await welcomeView.login();
    const singleDeviceWarningView = new SingleDeviceWarningView(driver);
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.continue();
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(driver);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skipRecovery();
    const mainView = new MainView(driver);
    await mainView.waitForDeviceDisplay(deviceName);
  },
  addRecoveryMechanismSeedPhrase: async (
    driver: ThenableWebDriver
  ): Promise<string> => {
    const mainView = new MainView(driver);
    await mainView.waitForDisplay();
    await mainView.addRecovery();

    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(driver);
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.useSeedPhrase();
    await recoveryMethodSelectorView.waitForSeedPhrase();
    const seedPhrase = await recoveryMethodSelectorView.getSeedPhrase();
    await recoveryMethodSelectorView.copySeedPhrase();
    await recoveryMethodSelectorView.seedPhraseContinue();

    return seedPhrase;
  },
};
