class View {
  constructor(protected browser: WebdriverIO.Browser) {}

  // There is something really odd happening with the "backdrop" design.
  // When the card's "::before" is bigger than 110%, wdio fails to click some buttons,
  // even though it's in view (and can e.g. be clicked on from the browser view).
  // Clicking from the browser directly fixes the issue.
  async click(selector: string) {
    const elem = await this.browser.$(selector);

    const click = (elem: WebdriverIO.Element) => {
      elem.click();
    };

    await this.browser.execute(click, elem);
  }
}

export class WelcomeView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#registerUserNumber")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async typeUserNumber(userNumber: string): Promise<void> {
    await this.browser.$("#registerUserNumber").setValue(userNumber);
  }

  async login(): Promise<void> {
    await this.click("#loginButton");
  }

  async register(): Promise<void> {
    await this.click("#registerButton");
  }

  async addDevice(): Promise<void> {
    await this.click("#addNewDeviceButton");
  }

  async recover(): Promise<void> {
    await this.click("#recoverButton");
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
    await this.click('button[type="submit"]');
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
    await this.click("#confirmRegisterButton");
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
    await this.click("#displayUserContinue");
  }

  async registerIdentityFixup(): Promise<void> {
    const elem = await this.browser.$("[data-usernumber]");
    await this.browser.execute(
      "arguments[0].innerText = arguments[1];",
      elem,
      "12345"
    );
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
    await this.click("#displayWarningAddRecovery");
  }

  async remindLater(): Promise<void> {
    // we need to scroll down in case of NOT headless, otherwise the button may not be visible
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    await this.click("#displayWarningRemindLater");
  }
}

export class RecoveryMethodSelectorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser.$("#skipRecovery").waitForDisplayed({ timeout: 10_000 });
  }

  async useSeedPhrase(): Promise<void> {
    await this.click("#seedPhrase");
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
    await this.click("#skipRecovery");
  }

  async copySeedPhrase(): Promise<void> {
    await this.click("#seedCopy");
  }

  async seedPhraseContinue(): Promise<void> {
    await this.click("#displaySeedPhraseContinue");
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
    await this.click("#addAdditionalDevice");
  }

  async logout(): Promise<void> {
    // we need to scroll down in case of NOT headless, otherwise the button may not be visible
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    await this.click("#logoutButton");
  }

  async addRecovery(): Promise<void> {
    await this.click("#addRecovery");
  }

  async fixup(): Promise<void> {
    const elem = await this.browser.$("[data-usernumber]");
    await this.browser.execute(
      "arguments[0].innerText = arguments[1];",
      elem,
      "12345"
    );
  }

  async deviceSettings(deviceName: string): Promise<void> {
    await this.click(
      `//div[string()='${deviceName}']/following-sibling::button`
    );
  }
}

export class DeviceSettingsView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser.$("#deviceSettings").waitForDisplayed();
  }

  async remove(): Promise<void> {
    await this.click("button[data-action='remove']");
  }

  async back(): Promise<void> {
    await this.click("button[data-action='back']");
  }

  async protect(seedPhrase: string): Promise<void> {
    await this.click("button[data-action='protect']");

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
    await this.click("#deviceAliasContinue");
  }
}

export class AddDeviceFlowSelectorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#cancelAddDevice")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async selectLocalDevice(): Promise<void> {
    await this.click("#local");
  }

  async selectRemoteDevice(): Promise<void> {
    await this.click("#remote");
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
    await this.click("#registerTentativeDeviceContinue");
  }
}

export class NotInRegistrationModeView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#deviceRegModeDisabledRetry")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async retry(): Promise<void> {
    await this.click("#deviceRegModeDisabledRetry");
  }
}

export class AddRemoteDeviceInstructionsView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#cancelAddRemoteDevice")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async cancel(): Promise<void> {
    await this.click("#cancelAddRemoteDevice");
  }

  async fixup(): Promise<void> {
    const elem = await this.browser.$("#timer");
    await this.browser.execute(
      "arguments[0].outerHTML = arguments[1];",
      elem,
      "--:--"
    );
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

  async fixup(): Promise<void> {
    const elem = await this.browser.$("#timer");
    await this.browser.execute(
      "arguments[0].outerHTML = arguments[1];",
      elem,
      "--:--"
    );
    const codeElem = await this.browser.$("#verificationCode");
    await this.browser.execute(
      "arguments[0].innerText = arguments[1];",
      codeElem,
      "123456"
    );
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
    await this.click("#verifyDevice");
  }

  async fixup(): Promise<void> {
    const elem = await this.browser.$("#timer");
    await this.browser.execute(
      "arguments[0].outerHTML = arguments[1];",
      elem,
      "--:--"
    );
  }
}

export class AuthenticateView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#authorizeButton")
      .waitForDisplayed({ timeout: 5_000 });
  }

  async expectPrefilledAnchorToBe(anchor: string): Promise<void> {
    expect(await this.browser.$("#userNumberInput").getValue()).toBe(anchor);
  }

  async expectAnchorInputField(): Promise<void> {
    await this.browser
      .$("#userNumberInput")
      .waitForDisplayed({ timeout: 5_000 });
  }

  async authenticate(): Promise<void> {
    await this.click("#authorizeButton");
  }

  async register(): Promise<void> {
    await this.click("#registerButton");
  }

  async switchToAnchorInput(): Promise<void> {
    await this.click("#userNumberInput");
    // deselect input box, so we do not get flaky screenshots due to the blinking cursor
    await this.browser.execute(
      "document.getElementById('authorizeButton').focus();"
    );
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
    await this.click("#login");
  }

  async fixup(): Promise<void> {
    const elem = await this.browser.$("[data-usernumber]");
    await this.browser.execute(
      "arguments[0].innerText = arguments[1];",
      elem,
      "12345"
    );
  }
}

export class AddIdentityAnchorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#addDeviceUserNumber")
      .waitForDisplayed({ timeout: 3_000 });
  }

  async continue(userNumber?: string): Promise<void> {
    if (userNumber !== undefined) {
      await fillText(this.browser, "addDeviceUserNumber", userNumber);
    }
    await this.click("#addDeviceUserNumberContinue");
  }

  async fixup(): Promise<void> {
    // replace the Identity Anchor for a reproducible screenshot
    const elem = await this.browser.$("#addDeviceUserNumber");
    await this.browser.execute(
      "arguments[0].value = arguments[1];",
      elem,
      "12345"
    );
  }
}

export class AboutView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("//h1[string()='About']")
      .waitForDisplayed({ timeout: 5_000 });
  }
}

export class CompatabilityNoticeView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#compatibilityNotice")
      .waitForDisplayed({ timeout: 3_000 });
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
    await this.click("#signinBtn");
  }

  async signout(): Promise<void> {
    await this.click("#signoutBtn");
  }
  async setMaxTimeToLive(mttl: BigInt): Promise<void> {
    await fillText(this.browser, "maxTimeToLive", String(mttl));
  }

  async setDerivationOrigin(derivationOrigin: string): Promise<void> {
    await fillText(this.browser, "derivationOrigin", derivationOrigin);
  }

  async whoami(replicaUrl: string, testCanister: string): Promise<string> {
    await fillText(this.browser, "hostUrl", replicaUrl);
    await fillText(this.browser, "canisterId", testCanister);
    await this.click("#whoamiBtn");
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
    await this.click(`#${mode}`);
    await this.click("#updateNewAlternativeOrigins");
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
    await this.click("#openIiWindowBtn");
  }

  async sendInvalidData(): Promise<void> {
    await this.click("#invalidDataBtn");
  }

  async sendIncompleteMessage(): Promise<void> {
    await this.click("#incompleteMessageBtn");
  }

  async sendValidMessage(): Promise<void> {
    await this.click("#validMessageBtn");
  }
}

export class RecoverView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$(`//h1[string()='Recover Identity Anchor']`)
      .waitForDisplayed({ timeout: 5_000 });
  }

  async enterIdentityAnchor(identityAnchor: string): Promise<void> {
    await this.browser.$("#userNumberInput").setValue(identityAnchor);
  }

  async continue(): Promise<void> {
    await this.click("#userNumberContinue");
  }

  // enter seed phrase view
  async waitForSeedInputDisplay(): Promise<void> {
    await this.browser
      .$(`//h1[string()='Your seed phrase']`)
      .waitForDisplayed({ timeout: 5_000 });
  }

  async enterSeedPhrase(seedPhrase: string): Promise<void> {
    await this.browser.$("#inputSeedPhrase").setValue(seedPhrase);
  }

  async enterSeedPhraseContinue(): Promise<void> {
    await this.click("#inputSeedPhraseContinue");
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
    await this.click(`#${questionAnchor} summary`);
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
    await this.click(".displayErrorDetail");
    return (
      await this.browser.$(".displayErrorDetail > pre:nth-child(2)")
    ).getHTML();
  }

  async continue(): Promise<void> {
    await this.click("#displayErrorPrimary");
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
