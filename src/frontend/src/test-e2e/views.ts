class View {
  constructor(protected browser: WebdriverIO.Browser) {}
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
    await this.browser.$("#loginButton").click();
  }

  async register(): Promise<void> {
    await this.browser.$("#registerButton").click();
  }

  async addDevice(): Promise<void> {
    await this.browser.$("#addNewDeviceButton").click();
  }

  async recover(): Promise<void> {
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
    await this.browser.$('button[type="submit"]').click();
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
    return await this.browser.$(".highlightBox").getText();
  }

  async registerConfirmIdentity(): Promise<void> {
    await this.browser.$("#displayUserContinue").click();
  }

  async registerIdentityFixup(): Promise<void> {
    const elem = await this.browser.$(".highlightBox");
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

  async addAdditionalDevice(): Promise<void> {
    await this.browser.$("#addAdditionalDevice").click();
  }

  async logout(): Promise<void> {
    await this.browser.$("#logoutButton").click();
  }

  async addRecovery(): Promise<void> {
    await this.browser.$("#addRecovery").click();
  }

  async fixup(): Promise<void> {
    const elem = await this.browser.$(".highlightBox");
    await this.browser.execute(
      "arguments[0].innerText = arguments[1];",
      elem,
      "12345"
    );
  }

  async removeDevice(deviceName: string): Promise<void> {
    await this.browser
      .$(`//div[string()='${deviceName}']/following-sibling::button`)
      .click();
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
    await this.browser.$("#verifyDevice").click();
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
    await this.browser.$("#authorizeButton").click();
  }

  async register(): Promise<void> {
    await this.browser.$("#registerButton").click();
  }

  async switchToAnchorInput(): Promise<void> {
    await this.browser.$("#editAnchorButton").click();
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
    return await this.browser.$(".highlightBox").getText();
  }

  async login(): Promise<void> {
    await this.browser.$("#login").click();
  }

  async fixup(): Promise<void> {
    const elem = await this.browser.$(".highlightBox");
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
    await this.browser.$("#addDeviceUserNumberContinue").click();
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
  }

  async getPrincipal(): Promise<string> {
    return await this.browser.$("#principal").getText();
  }

  async signin(): Promise<void> {
    await this.browser.$("#signinBtn").click();
  }

  async setMaxTimeToLive(mttl: BigInt): Promise<void> {
    await fillText(this.browser, "maxTimeToLive", String(mttl));
  }

  async whoami(replicaUrl: string, whoamiCanister: string): Promise<string> {
    await fillText(this.browser, "hostUrl", replicaUrl);
    await fillText(this.browser, "canisterId", whoamiCanister);
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
    await this.browser.$("#userNumberInput").setValue(identityAnchor);
  }

  async continue(): Promise<void> {
    await this.browser.$("#userNumberContinue").click();
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
    await this.browser.$("#inputSeedPhraseContinue").click();
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

async function fillText(
  browser: WebdriverIO.Browser,
  id: string,
  text: string
): Promise<void> {
  const elem = await browser.$(`#${id}`);
  await elem.clearValue();
  await elem.setValue(text);
}
