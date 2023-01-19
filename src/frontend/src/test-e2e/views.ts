class View {
  constructor(protected browser: WebdriverIO.Browser) {}
}

export class WelcomeView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#registerButton")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async typeUserNumber(userNumber: string): Promise<void> {
    await this.browser.$('[data-role="anchor-input"]').setValue(userNumber);
  }

  async login(): Promise<void> {
    await this.browser.$("#loginButton").click();
  }

  async register(): Promise<void> {
    await this.browser.$("#registerButton").click();
  }

  async addDevice(): Promise<void> {
    await this.browser.$("#loginButton").click();
    await this.browser.$("#addNewDeviceButton").click();
  }

  async recover(): Promise<void> {
    await this.browser.$("#loginButton").click();
    await this.browser.$("#recoverButton").click();
  }
}

export class RegisterView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#registerAlias")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async enterAlias(alias: string): Promise<void> {
    await this.browser.$("#registerAlias").setValue(alias);
  }

  async create(): Promise<void> {
    await this.browser.$("#registerButton").click();
  }

  // View: Register confirmation
  async waitForRegisterConfirm(): Promise<void> {
    await this.browser
      .$("#confirmRegisterButton")
      .waitForDisplayed({ timeout: 25_000 });
    await this.browser.$("#captchaInput").waitForDisplayed({ timeout: 25_000 });
  }

  async confirmRegisterConfirm(): Promise<void> {
    await this.browser.$("#captchaInput").waitForEnabled({ timeout: 40_000 });
    // In tests, the captchas are hard-coded to the following string: "a"
    await this.browser.$("#captchaInput").setValue("a");
    await this.browser
      .$("#confirmRegisterButton")
      // this is a huge timeout because generating the captcha takes a while on
      // the emulator
      .waitForEnabled({ timeout: 30_000 });
    await this.browser.$("#confirmRegisterButton").click();
  }

  // View: Register Show Number
  async waitForIdentity(): Promise<void> {
    await this.browser
      .$("#displayUserContinue")
      .waitForDisplayed({ timeout: 15_000 });
  }

  async registerGetIdentity(): Promise<string> {
    return await this.browser.$("[data-usernumber]").getText();
  }

  async registerConfirmIdentity(): Promise<void> {
    await this.browser.$("#displayUserContinue").click();
  }
}

export class SingleDeviceWarningView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#displayWarningAddRecovery")
      .waitForDisplayed({ timeout: 10_000 });
    await this.browser
      .$("#displayWarningRemindLater")
      .waitForDisplayed({ timeout: 1_000 });
  }

  async addRecovery(): Promise<void> {
    // we need to scroll down in case of NOT headless, otherwise the button may not be visible
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    await this.browser.$("#displayWarningAddRecovery").click();
  }

  async remindLater(): Promise<void> {
    // we need to scroll down in case of NOT headless, otherwise the button may not be visible
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    await this.browser.$("#displayWarningRemindLater").click();
  }
}

export class RecoveryMethodSelectorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser.$("#skipRecovery").waitForDisplayed({ timeout: 10_000 });
  }

  async useSeedPhrase(): Promise<void> {
    await this.browser.$("#seedPhrase").click();
  }

  async waitForSeedPhrase(): Promise<void> {
    await this.browser
      .$("//h1[string()='Seedphrase']")
      .waitForDisplayed({ timeout: 15_000 });
  }

  async getSeedPhrase(): Promise<string> {
    return await this.browser.$("#seedPhrase").getText();
  }

  async skipRecovery(): Promise<void> {
    await this.browser.$("#skipRecovery").click();
  }

  async copySeedPhrase(): Promise<void> {
    await this.browser.$("#seedCopy").click();
  }

  async acknowledgeCheckbox(): Promise<void> {
    await this.browser.$("#ack-checkbox").click();
  }

  async seedPhraseContinue(): Promise<void> {
    await this.browser.$("#displaySeedPhraseContinue").click();
  }
}

export class MainView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("//h1[string()='Anchor Management']")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async waitForDeviceDisplay(deviceName: string): Promise<void> {
    await this.browser
      .$(`//div[string()='${deviceName}']`)
      .waitForDisplayed({ timeout: 10_000 });
  }

  async waitForDeviceNotDisplay(deviceName: string): Promise<void> {
    await this.browser
      .$(`//div[string()='${deviceName}']`)
      .waitForDisplayed({ timeout: 10_000, reverse: true });
  }

  async addAdditionalDevice(): Promise<void> {
    await this.browser.$("#addAdditionalDevice").click();
  }

  async logout(): Promise<void> {
    // we need to scroll down in case of NOT headless, otherwise the button may not be visible
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    await this.browser.$("#logoutButton").click();
  }

  async addRecovery(): Promise<void> {
    await this.browser.$("#addRecovery").click();
  }

  async deviceSettings(deviceName: string): Promise<void> {
    await this.browser
      .$(`//div[string()='${deviceName}']/following-sibling::button`)
      .click();
  }
}

export class DeviceSettingsView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser.$("#deviceSettings").waitForDisplayed();
  }

  async remove(): Promise<void> {
    await this.browser.$("button[data-action='remove']").click();
  }

  async back(): Promise<void> {
    await this.browser.$("button[data-action='back']").click();
  }

  async protect(seedPhrase: string): Promise<void> {
    await this.browser.$("button[data-action='protect']").click();

    const recoveryView = new RecoverView(this.browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(seedPhrase);
    await recoveryView.enterSeedPhraseContinue();
  }

  async removeNotDisplayed(): Promise<void> {
    await this.browser
      .$("button[data-action='remove']")
      .waitForDisplayed({ reverse: true });
  }
}

export class AddDeviceAliasView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#deviceAliasContinue")
      .waitForDisplayed({ timeout: 3_000 });
  }

  async addAdditionalDevice(alias: string): Promise<void> {
    await this.browser.$("#deviceAlias").setValue(alias);
  }

  async continue(): Promise<void> {
    await this.browser.$("#deviceAliasContinue").click();
  }
}

export class AddDeviceFlowSelectorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#cancelAddDevice")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async selectLocalDevice(): Promise<void> {
    await this.browser.$("#local").click();
  }

  async selectRemoteDevice(): Promise<void> {
    await this.browser.$("#remote").click();
  }
}

export class AddRemoteDeviceAliasView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#registerTentativeDeviceContinue")
      .waitForDisplayed({ timeout: 5_000 });

    // Make sure the loader is gone
    await this.browser.$("#loader").waitForExist({ reverse: true });
  }

  async selectAlias(alias: string): Promise<void> {
    await this.browser.$("#tentativeDeviceAlias").setValue(alias);
  }

  async continue(): Promise<void> {
    // we need to scroll down in case of NOT headless, otherwise the button may not be visible
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    await this.browser.$("#registerTentativeDeviceContinue").click();
  }
}

export class NotInRegistrationModeView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#deviceRegModeDisabledRetry")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async retry(): Promise<void> {
    await this.browser.$("#deviceRegModeDisabledRetry").click();
  }
}

export class AddRemoteDeviceInstructionsView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#cancelAddRemoteDevice")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async cancel(): Promise<void> {
    await this.browser.$("#cancelAddRemoteDevice").click();
  }
}

export class AddRemoteDeviceVerificationCodeView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#verificationCode")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async getVerificationCode(): Promise<string> {
    return await this.browser.$("#verificationCode").getText();
  }
}

export class VerifyRemoteDeviceView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser.$("#verifyDevice").waitForDisplayed({ timeout: 5_000 });
  }

  async enterVerificationCode(code: string): Promise<void> {
    await this.browser.$("#tentativeDeviceCode").setValue(code);
  }

  async continue(): Promise<void> {
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    await this.browser.$("#verifyDevice").click();
  }
}

export class AuthenticateView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$('[data-page="authenticate"]')
      .waitForExist({ timeout: 5_000 });
  }

  async pickAnchor(anchor: string): Promise<void> {
    await this.browser.$(`[data-anchor-id="${anchor}"]`).click();
  }

  async expectAnchorInputField(): Promise<void> {
    await this.browser
      .$('[data-role="anchor-input"]')
      .waitForDisplayed({ timeout: 5_000 });
  }

  async authenticate(): Promise<void> {
    const moreOptions = await this.browser.$('[data-role="more-options"]');
    if (await moreOptions.isExisting()) {
      await moreOptions.click();
    }
    await this.browser.$("#authorizeButton").click();
  }

  async register(): Promise<void> {
    await this.browser.$("#registerButton").click();
  }

  async switchToAnchorInput(): Promise<void> {
    await this.browser.$('[data-role="anchor-input"]').click();
  }
}

export class WelcomeBackView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#loginDifferent")
      .waitForDisplayed({ timeout: 15_000 });
  }

  async getIdentityAnchor(): Promise<string> {
    return await this.browser.$("[data-usernumber]").getText();
  }

  async login(): Promise<void> {
    await this.browser.$("#login").click();
  }
}

export class AddIdentityAnchorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$('[data-role="anchor-input"]')
      .waitForDisplayed({ timeout: 3_000 });
  }

  async continue(userNumber?: string): Promise<void> {
    if (userNumber !== undefined) {
      await this.browser.$('[data-role="anchor-input"]').setValue(userNumber);
    }
    await this.browser.$("#userNumberContinue").click();
  }
}

export class AboutView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("//h1[string()='About']")
      .waitForDisplayed({ timeout: 5_000 });
  }
}

export class DemoAppView extends View {
  async open(demoAppUrl: string, iiUrl: string): Promise<void> {
    await this.browser.url(demoAppUrl);
    await fillText(this.browser, "iiUrl", iiUrl);
  }

  async waitForDisplay(): Promise<void> {
    await this.browser.$("#principal").waitForDisplayed({ timeout: 10_000 });
    // wait for the slowest element to appear
    await this.browser.waitUntil(
      async () =>
        (await this.browser.$("#alternativeOrigins").getText()) !== "",
      { timeoutMsg: "alternativeOrigins were not displayed" }
    );
  }

  async getPrincipal(): Promise<string> {
    return await this.browser.$("#principal").getText();
  }

  async signin(): Promise<void> {
    await this.browser.$("#signinBtn").click();
  }

  async signout(): Promise<void> {
    await this.browser.$("#signoutBtn").click();
  }
  async setMaxTimeToLive(mttl: bigint): Promise<void> {
    await fillText(this.browser, "maxTimeToLive", String(mttl));
  }

  async setDerivationOrigin(derivationOrigin: string): Promise<void> {
    await fillText(this.browser, "derivationOrigin", derivationOrigin);
  }

  async whoami(replicaUrl: string, testCanister: string): Promise<string> {
    await fillText(this.browser, "hostUrl", replicaUrl);
    await fillText(this.browser, "canisterId", testCanister);
    await this.browser.$("#whoamiBtn").click();
    const whoamiResponseElem = await this.browser.$("#whoamiResponse");
    await whoamiResponseElem.waitUntil(
      async () => {
        return (await whoamiResponseElem.getText()).indexOf("-") !== -1;
      },
      {
        timeout: 6_000,
        timeoutMsg: 'expected whoami response to contain "-" for 6s',
      }
    );
    return await whoamiResponseElem.getText();
  }

  async updateAlternativeOrigins(
    replicaUrl: string,
    testCanister: string,
    alternativeOrigins: string,
    mode: "certified" | "uncertified" | "redirect"
  ): Promise<string> {
    await fillText(this.browser, "hostUrl", replicaUrl);
    await fillText(this.browser, "canisterId", testCanister);
    await fillText(this.browser, "newAlternativeOrigins", alternativeOrigins);
    await this.browser.$(`#${mode}`).click();
    await this.browser.$("#updateNewAlternativeOrigins").click();
    const alternativeOriginsElem = await this.browser.$("#alternativeOrigins");
    await alternativeOriginsElem.waitUntil(
      async () => {
        return (await alternativeOriginsElem.getText()) === alternativeOrigins;
      },
      {
        timeout: 6_000,
        timeoutMsg: "expected alternativeOrigins to update within 6s",
      }
    );
    return await alternativeOriginsElem.getText();
  }

  async resetAlternativeOrigins(
    replicaUrl: string,
    testCanister: string
  ): Promise<string> {
    return this.updateAlternativeOrigins(
      replicaUrl,
      testCanister,
      '{"alternativeOrigins":[]}',
      "certified"
    );
  }

  async getMessageText(messageNo: number): Promise<string> {
    return await this.browser
      .$(`div.postMessage:nth-child(${messageNo}) > div:nth-child(2)`)
      .getText();
  }

  async waitForNthMessage(messageNo: number): Promise<void> {
    await this.browser
      .$(`div.postMessage:nth-child(${messageNo})`)
      .waitForDisplayed();
  }

  async openIiTab(): Promise<void> {
    await this.browser.$("#openIiWindowBtn").click();
  }

  async sendInvalidData(): Promise<void> {
    await this.browser.$("#invalidDataBtn").click();
  }

  async sendIncompleteMessage(): Promise<void> {
    await this.browser.$("#incompleteMessageBtn").click();
  }

  async sendValidMessage(): Promise<void> {
    await this.browser.$("#validMessageBtn").click();
  }
}

export class RecoverView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$(`//h1[string()='Recover Identity Anchor']`)
      .waitForDisplayed({ timeout: 5_000 });
  }

  async enterIdentityAnchor(identityAnchor: string): Promise<void> {
    await this.browser.$('[data-role="anchor-input"]').setValue(identityAnchor);
  }

  async continue(): Promise<void> {
    await this.browser.$("#userNumberContinue").click();
  }

  // enter seed phrase view
  async waitForSeedInputDisplay(): Promise<void> {
    await this.browser
      .$(
        `//h1[string()='Provide your Recovery Phrase to recover your Internet Identity']`
      )
      .waitForDisplayed({ timeout: 5_000 });
  }

  async enterSeedPhrase(seedPhrase: string): Promise<void> {
    await this.browser.$("#inputSeedPhrase").setValue(seedPhrase);
  }

  async enterSeedPhraseContinue(): Promise<void> {
    await this.browser.$("#inputSeedPhraseContinue").click();
  }

  async waitForInvalidSeedPhraseDisplay(): Promise<void> {
    await this.browser.$("h3=Invalid Seed Phrase").waitForDisplayed();
  }
}

export class FAQView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("//h1[string()='FAQ']")
      .waitForDisplayed({ timeout: 5_000 });
  }

  async openQuestion(questionAnchor: string): Promise<void> {
    await this.browser.$(`#${questionAnchor} summary`).click();
  }
}

export class ErrorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#errorContainer")
      .waitForDisplayed({ timeout: 5_000 });
  }

  async getErrorMessage(): Promise<string> {
    return this.browser.$("[data-role='warning-message']").getText();
  }

  async getErrorDetail(): Promise<string> {
    return (await this.browser.$('[data-role="error-detail"]')).getText();
  }

  async continue(): Promise<void> {
    await this.browser.$("#displayErrorPrimary").click();
  }
}

async function fillText(
  browser: WebdriverIO.Browser,
  id: string,
  text: string
): Promise<void> {
  const elem = await browser.$(`#${id}`);
  await elem.clearValue();
  await elem.setValue(text);
}
