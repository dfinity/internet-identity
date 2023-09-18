import { zip } from "$src/utils/utils";
import { nonNullish } from "@dfinity/utils";

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
    await this.browser.$("#loginButton").waitForDisplayed();
    await this.browser.$("#loginButton").scrollIntoView();
    await this.browser.$("#loginButton").click();
  }

  async register(): Promise<void> {
    await this.browser.$("#registerButton").waitForDisplayed();
    await this.browser.$("#registerButton").scrollIntoView();
    await this.browser.$("#registerButton").click();
  }

  async addDevice(): Promise<void> {
    await this.browser.$("#loginButton").click();
    await this.browser.$("#addNewDeviceButton").click();
  }

  async recover(): Promise<void> {
    await this.browser.$("#loginButton").waitForDisplayed();
    await this.browser.$("#loginButton").scrollIntoView();
    await this.browser.$("#loginButton").click();
    await this.browser.$("#recoverButton").waitForDisplayed();
    await this.browser.$("#recoverButton").scrollIntoView();
    await this.browser.$("#recoverButton").click();
    await this.browser.$('[data-action="recover-with-phrase"]').click();
  }
}

export class RenameView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#pickAliasInput")
      .waitForDisplayed({ timeout: 10_000 });
  }

  async enterAlias(alias: string): Promise<void> {
    await this.browser.$("#pickAliasInput").setValue(alias);
  }

  async submit(): Promise<void> {
    await this.browser.$("#pickAliasSubmit").click();
  }
}

export class RegisterView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$('[data-action="construct-identity"')
      .waitForDisplayed({ timeout: 10_000 });
  }

  async create(): Promise<void> {
    await this.browser.$('[data-action="construct-identity"').click();
  }

  async createPin(): Promise<void> {
    await this.browser.$('[data-action="construct-pin-identity"').click();
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

  async assertPinRegistrationNotShown(): Promise<void> {
    await this.browser
      .$('[data-action="construct-pin-identity"')
      .waitForDisplayed({ reverse: true });
  }
}

export class PinRegistrationView extends View {
  async waitForPinInfo(): Promise<void> {
    await this.browser
      .$('[data-action="continue-pin"]')
      .waitForDisplayed({ timeout: 10_000 });
  }

  async pinInfoContinue(): Promise<void> {
    await this.browser.$('[data-action="continue-pin"').click();
  }
  async waitForSetPin(): Promise<void> {
    await this.browser
      .$('[data-role="set-pin"]')
      .waitForDisplayed({ timeout: 10_000 });
  }

  async setPin(pin: string): Promise<void> {
    const inputs = await this.browser.$('[data-role="set-pin"]').$$("input");
    for (const [input, digit] of zip(inputs, pin.split(""))) {
      await input.setValue(digit);
    }
  }
  async waitForConfirmPin(): Promise<void> {
    await this.browser
      .$('[data-role="confirm-pin"]')
      .waitForDisplayed({ timeout: 10_000 });
  }

  async confirmPin(pin: string): Promise<void> {
    const inputs = await this.browser
      .$('[data-role="confirm-pin"]')
      .$$("input");
    for (const [input, digit] of zip(inputs, pin.split(""))) {
      await input.setValue(digit);
    }
  }
}

export class RecoveryMethodSelectorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$('[data-action="cancel"]')
      .waitForDisplayed({ timeout: 10_000 });
  }

  async waitForSeedPhrase(): Promise<void> {
    await this.browser
      .$('[data-role="recovery-words"]')
      .waitForDisplayed({ timeout: 15_000 });
  }

  async getSeedPhrase(): Promise<string> {
    // This tries to read the recovery phrase by first copying it to the clipboard.

    await this.copySeedPhrase();

    // Our CSP policy prevents us from directly reading the clipboard.
    // Instead, we mock user input to paste the clipboard content in textarea element and
    // read the element's value.

    // First, create a new textarea element where the phrase will be pasted
    await this.browser.execute(() => {
      const elem = document.createElement("textarea");
      elem.setAttribute("id", "my-paste-area");
      document.body.prepend(elem);
    });

    // Select the element and mock "Ctrl + V" for pasting the clipboard content into said element
    await this.browser.$("#my-paste-area").click();
    await this.browser.keys(["Control", "v"]);

    // Read the element's value and clean up
    const seedPhrase = await this.browser.execute(() => {
      const elem = document.querySelector(
        "#my-paste-area"
      ) as HTMLTextAreaElement;
      // NOTE: we could also query the value with wdio's $(..).getValue(), but since we have
      // the element here might as well.
      const seedPhrase = elem.value!;
      elem.remove();
      return seedPhrase;
    });

    return seedPhrase;
  }

  async skipRecovery(): Promise<void> {
    await this.browser.$('[data-action="cancel"]').click();
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

  async seedPhraseFill(): Promise<void> {
    await this.browser.$('[data-action="next"]').waitForDisplayed();
    const missings = await this.browser.$$("[data-expected]");
    for await (const missing of missings) {
      const expected = await missing.getAttribute("data-expected");
      await missing.setValue(expected);
    }
    await this.browser.$('[data-action="next"]').click();
  }
}

export class MainView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$('[data-role="identity-management"]')
      .waitForDisplayed({ timeout: 10_000 });
  }

  async waitForDeviceCount(deviceName: string, count: number): Promise<void> {
    const elems = await this.browser.$$(`//li[@data-device="${deviceName}"]`);
    if (elems.length !== count) {
      throw Error("Bad number of elements");
    }
  }

  async waitForDeviceDisplay(deviceName: string): Promise<void> {
    await this.browser
      .$(`//li[@data-device="${deviceName}"]`)
      .waitForDisplayed({ timeout: 10_000 });
  }

  async waitForDeviceNotDisplay(deviceName: string): Promise<void> {
    await this.browser
      .$(`//li[@data-device="${deviceName}"]`)
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
    await this.browser.$('[data-action="add-recovery-phrase"]').click();
    await this.browser.$('[data-page="add-recovery-phrase"]').waitForExist();
    await this.browser.$('[data-action="next"]').click();
  }

  async rename(deviceName: string, newName: string): Promise<void> {
    await this.openDeviceActions({ deviceName });
    await this.deviceAction({ deviceName, action: "rename" }).click();

    const renameView = new RenameView(this.browser);
    await renameView.waitForDisplay();
    await renameView.enterAlias(newName);
    await renameView.submit();
  }

  async protect(deviceName: string, seedPhrase: string): Promise<void> {
    await this.openDeviceActions({ deviceName });
    await this.deviceAction({ deviceName, action: "protect" }).click();
    await this.browser.$('[data-page="protect-phrase-info"]').waitForExist();
    await this.browser.$('[data-action="next"]').click();

    const recoveryView = new RecoverView(this.browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(seedPhrase);
    await recoveryView.enterSeedPhraseContinue();
  }

  async assertDeviceProtected(deviceName: string): Promise<void> {
    await this.browser
      .$(`//li[@data-device="${deviceName}"]/div[@data-role="protected"]`)
      .waitForDisplayed({ timeout: 10_000 });
  }

  async unprotect(deviceName: string, seedPhrase: string): Promise<void> {
    await this.openDeviceActions({ deviceName });
    await this.deviceAction({ deviceName, action: "unprotect" }).click();
    await this.browser.$('[data-page="unprotect-phrase-info"]').waitForExist();
    await this.browser.$('[data-action="next"]').click();

    const recoveryView = new RecoverView(this.browser);
    await recoveryView.waitForSeedInputDisplay();
    await recoveryView.enterSeedPhrase(seedPhrase);
    await recoveryView.enterSeedPhraseContinue();
  }

  async assertDeviceUnprotected(deviceName: string): Promise<void> {
    await this.browser
      .$(`//li[@data-device="${deviceName}"]/div[@data-role="protected"]`)
      .waitForDisplayed({ timeout: 10_000, reverse: true });
  }

  async reset(deviceName: string): Promise<void> {
    await this.openDeviceActions({ deviceName });
    await this.deviceAction({ deviceName, action: "reset" }).click();
    await this.browser.$('[data-page="reset-phrase-info"]').waitForExist();
    await this.browser.$('[data-action="next"]').click();
  }

  async removeNotDisplayed(deviceName: string): Promise<void> {
    await this.openDeviceActions({ deviceName });
    await this.deviceAction({ deviceName, action: "remove" }).waitForDisplayed({
      reverse: true,
    });
  }

  // Open the device settings/actions dropdown
  async openDeviceActions({
    deviceName,
  }: {
    deviceName: string;
  }): Promise<void> {
    // Ensure the settings dropdown is in view
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    // Grab the trigger (button) and figure out the id of the element it opens
    const dropdownTrigger = await this.browser.$(
      `[data-action="open-settings"][data-device="${deviceName}"]`
    );
    const dropdownId = await dropdownTrigger.getAttribute("aria-controls");
    await dropdownTrigger.click();

    // The menu element has an animation, so we wait until all animations are finished so
    // that we can reliably click on its elements
    await this.browser.waitUntil(
      () =>
        this.browser.execute(
          (dropdownId) =>
            document.getElementById(dropdownId)?.getAnimations().length === 0,
          dropdownId
        ),
      { timeoutMsg: "Animation didn't end" }
    );
  }

  // Get a device action element from the device settings menu (menu must be opened
  // separately)
  deviceAction({ deviceName, action }: { deviceName: string; action: string }) {
    return this.browser.$(
      `[data-action="${action}"][data-device="${deviceName}"]`
    );
  }
}

export class AddRemoteDeviceAliasView extends View {
  async waitForDisplay(): Promise<void> {
    await this.browser
      .$("#pickAliasSubmit")
      .waitForDisplayed({ timeout: 5_000 });

    // Make sure the loader is gone
    await this.browser.$("#loader").waitForExist({ reverse: true });
  }

  async selectAlias(alias: string): Promise<void> {
    await this.browser.$("#pickAliasInput").setValue(alias);
  }

  async continue(): Promise<void> {
    // we need to scroll down in case of NOT headless, otherwise the button may not be visible
    await this.browser.execute(
      "window.scrollTo(0, document.body.scrollHeight)"
    );
    await this.browser.$("#pickAliasSubmit").click();
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

  async addFIDODevice(): Promise<void> {
    await this.browser.$('[data-action="use-fido"]').click();
  }

  async addDeviceLink(): Promise<string> {
    return await this.browser.$(`[data-role="add-device-link"]`).getText();
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
    const inputs = await this.browser
      .$('[data-role="verification-code"]')
      .$$("input");
    for (const [input, digit] of zip(inputs, code.split(""))) {
      await input.setValue(digit);
    }
  }

  async continue(): Promise<void> {
    await this.browser.$("#verifyDevice").click();
  }
}

export class AddDeviceSuccessView extends View {
  private readonly SELECTOR = "[data-action='next']";

  async waitForDisplay(): Promise<void> {
    await this.browser.$(this.SELECTOR).waitForDisplayed({ timeout: 5_000 });
  }

  async continue(): Promise<void> {
    await this.browser.$(this.SELECTOR).click();
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
    if (nonNullish(userNumber)) {
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
  // enter seed phrase view
  async waitForSeedInputDisplay(): Promise<void> {
    await this.browser
      .$('[data-page="recover-with-phrase"]')
      .waitForDisplayed({ timeout: 5_000 });
  }

  async enterSeedPhrase(seedPhrase: string): Promise<void> {
    const words = seedPhrase.split(" ").filter(Boolean);

    // Assume the only inputs on this screen are the phrase inputs
    const inputs = await this.browser.$$("input");
    for (let i = 0; i < words.length; i++) {
      await inputs[i].setValue(words[i]);
    }
  }

  async enterSeedPhraseContinue(): Promise<void> {
    await this.browser.$('[data-action="next"]').click();
  }

  async skipDeviceEnrollment(): Promise<void> {
    await this.browser.$("#pickAliasCancel").click();
  }

  async waitForInvalidSeedPhraseDisplay(): Promise<void> {
    await this.browser.$("p*=Could not use recovery phrase").waitForDisplayed();
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
