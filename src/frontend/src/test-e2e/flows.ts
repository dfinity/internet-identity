import { ThenableWebDriver } from "selenium-webdriver";
import {
  MainView,
  RecoveryMethodSelector,
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
    const registerView = new RegisterView(driver);
    const singleDeviceWarningView = new SingleDeviceWarningView(driver);
    const recoveryMethodSelector = new RecoveryMethodSelector(driver);

    await welcomeView.waitForDisplay();
    await welcomeView.register();
    await registerView.waitForDisplay();
    await registerView.enterAlias(deviceName);
    await registerView.create();
    await registerView.waitForRegisterConfirm();
    await registerView.confirmRegisterConfirm();
    await registerView.waitForIdentity();
    const userNumber = await registerView.registerGetIdentity();
    await registerView.registerConfirmIdentity();
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.continue();
    await recoveryMethodSelector.waitForDisplay();
    await recoveryMethodSelector.skip();
    return userNumber;
  },
  login: async (
    userNumber: string,
    deviceName: string,
    driver: ThenableWebDriver
  ): Promise<void> => {
    const welcomeView = new WelcomeView(driver);
    const singleDeviceWarningView = new SingleDeviceWarningView(driver);
    const recoveryMethodSelector = new RecoveryMethodSelector(driver);
    const mainView = new MainView(driver);

    await welcomeView.waitForDisplay();
    await welcomeView.typeUserNumber(userNumber);
    await welcomeView.login();
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.continue();
    await recoveryMethodSelector.waitForDisplay();
    await recoveryMethodSelector.skip();
    await mainView.waitForDeviceDisplay(deviceName);
  },
};
