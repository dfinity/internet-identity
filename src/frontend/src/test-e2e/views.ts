import { By, ThenableWebDriver, until } from "selenium-webdriver";

class View {
  constructor(protected driver: ThenableWebDriver) {}
}

export class WelcomeView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("registerUserNumber")), 10_000);
  }

  async typeUserNumber(userNumber: string): Promise<void> {
    await this.driver.findElement(By.id("registerUserNumber")).sendKeys(userNumber);
  }

  async login(): Promise<void> {
    await this.driver.findElement(By.id("loginButton")).click();
  }

  async register(): Promise<void> {
    await this.driver.findElement(By.id("registerButton")).click();
  }

  async addDevice(): Promise<void> {
    await this.driver.findElement(By.id("addNewDeviceButton")).click();
  }
}

export class RegisterView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("registerAlias")), 10_000);
  }

  async enterAlias(alias: string): Promise<void> {
    await this.driver.findElement(By.id("registerAlias")).sendKeys(alias);
  }

  async create(): Promise<void> {
    await this.driver.findElement(By.css("button[type=\"submit\"]")).click();
  }

  // View: Register confirmation
  async waitForRegisterConfirm(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("confirmRegisterButton")), 25_000);
  }

  async confirmRegisterConfirm(): Promise<void> {
    await this.driver.findElement(By.id("confirmRegisterButton")).click();
  }

  // View: Register Show Number
  async waitForIdentity(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("displayUserContinue")), 15_000);
  }

  async registerGetIdentity(): Promise<string> {
    return await this.driver.findElement(By.className("highlightBox")).getText();
  }

  async registerConfirmIdentity(): Promise<void> {
    await this.driver.findElement(By.id("displayUserContinue")).click();
  }

  async registerIdentityFixup(): Promise<void> {
    const elem = await this.driver.findElement(By.className("highlightBox"));
    await this.driver.executeScript(
      "arguments[0].innerText = arguments[1];",
      elem,
      "12345"
    );
  }
}

export class SingleDeviceWarningView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("displayWarningPrimary")), 3_000);
  }

  async continue(): Promise<void> {
    // we need to scroll down in case of NOT headless, otherwise the button may not be visible
    await this.driver.executeScript("window.scrollTo(0, document.body.scrollHeight)");
    await this.driver.findElement(By.id("displayWarningPrimary")).click();
  }
}

export class RecoveryMethodSelector extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("skipRecovery")), 3_000);
  }

  async skip(): Promise<void> {
    await this.driver.findElement(By.id("skipRecovery")).click();
  }
}

export class MainView extends View {
  async waitForDeviceDisplay(deviceName: string): Promise<void> {
    await this.driver.wait(until.elementLocated(By.xpath(`//div[string()='${deviceName}']`)), 10_000);
  }

  async addAdditionalDevice(): Promise<void> {
    await this.driver.findElement(By.id("addAdditionalDevice")).click();
  }

  async logout(): Promise<void> {
    await this.driver.findElement(By.id("logoutButton")).click();
  }

  async fixup(): Promise<void> {
    const elem = await this.driver.findElement(By.className("highlightBox"));
    await this.driver.executeScript(
      "arguments[0].innerText = arguments[1];",
      elem,
      "12345"
    );
  }

  async removeDevice(deviceName: string): Promise<void> {
    await this.driver.findElement(By.xpath(`//div[string()='${deviceName}']/following-sibling::button`)).click();
  }
}

export class AddDeviceAliasView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("deviceAliasContinue")), 3_000);
  }

  async addAdditionalDevice(alias: string): Promise<void> {
    await this.driver.findElement(By.id("deviceAlias")).sendKeys(alias);
  }

  async continue(): Promise<void> {
    await this.driver.findElement(By.id("deviceAliasContinue")).click();
  }
}

export class AuthorizeAppView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("confirmRedirect")), 5_000);
  }

  async confirm(): Promise<void> {
    await this.driver.findElement(By.id("confirmRedirect")).click();
  }
}

export class WelcomeBackView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("loginDifferent")), 15_000);
  }

  async getIdentityAnchor(): Promise<string> {
    return await this.driver.findElement(By.className("highlightBox")).getText();
  }

  async login(): Promise<void> {
    await this.driver.findElement(By.id("login")).click();
  }

  async fixup(): Promise<void> {
    const elem = await this.driver.findElement(By.className("highlightBox"));
    await this.driver.executeScript(
      "arguments[0].innerText = arguments[1];",
      elem,
      "12345"
    );
  }
}

export class AddIdentityAnchorView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("addDeviceUserNumber")), 3_000)
  }

  async continue(userNumber?: string): Promise<void> {
    if (userNumber !== undefined) {
      await fillText(this.driver, "addDeviceUserNumber", userNumber);
    }
    await this.driver.findElement(By.id("addDeviceUserNumberContinue")).click();
  }

  async fixup(): Promise<void> {
    // replace the Identity Anchor for a reproducible screenshot
    const elem = await this.driver.findElement(By.id("addDeviceUserNumber"));
    await this.driver.executeScript(
      "arguments[0].value = arguments[1];",
      elem,
      "12345"
    );
  }
}

export class AddDeviceView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("linkText")), 3_000)
  }

  async getLinkText(): Promise<string> {
    return await this.driver.findElement(By.id("linkText")).getAttribute("value");
  }

  async fixup(): Promise<void> {
    const elem = await this.driver.wait(
      until.elementLocated(By.id("linkText")),
      3_000
    );
    await this.driver.executeScript(
      "arguments[0].value = arguments[1];",
      elem,
      "(link removed from screenshot)"
    );
  }

  // View: Add device confirm
  async waitForConfirmDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("addDevice")), 3_000)
  }

  async confirm(): Promise<void> {
    await this.driver.findElement(By.id("addDevice")).click();
  }

  async fixupConfirm(): Promise<void> {
    const userNumberElem = await this.driver.findElement(By.className("highlightBox"));
    await this.driver.executeScript(
      "arguments[0].innerText = arguments[1];",
      userNumberElem,
      "12345"
    );
  }

  // View: Add device alias
  async waitForAliasDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("deviceAliasContinue")), 3_000)
  }

  async addDeviceAlias(alias: string): Promise<void> {
    await this.driver.findElement(By.id("deviceAlias")).sendKeys(alias);
  }

  async addDeviceAliasContinue(): Promise<void> {
    await this.driver.findElement(By.id("deviceAliasContinue")).click();
  }

  // View: Add device success
  async waitForAddDeviceSuccess(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("manageDevicesButton")), 10_000)
  }
}


export class AboutView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("about")), 3_000);
  }
}


export class CompatabilityNoticeView extends View {
  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("compatibilityNotice")), 3_000);
  }
}

export class DemoAppView extends View {

  async open(demoAppUrl: string, iiUrl: string): Promise<void> {
    await this.driver.get(demoAppUrl);
    await fillText(this.driver, "iiUrl", iiUrl);
  }

  async waitForDisplay(): Promise<void> {
    await this.driver.wait(until.elementLocated(By.id("principal")), 10_000);
  }

  async getPrincipal(): Promise<string> {
    return await this.driver.findElement(By.id("principal")).getText();
  }

  async signin(): Promise<void> {
    await this.driver.findElement(By.id("signinBtn")).click();
  }

  async setMaxTimeToLive(mttl: BigInt): Promise<void> {
    await fillText(this.driver, "maxTimeToLive", String(mttl));
  }

  async whoami(replicaUrl: string, whoamiCanister: string): Promise<string> {
    await fillText(this.driver, "hostUrl", replicaUrl);
    await fillText(this.driver, "canisterId", whoamiCanister);
    await this.driver.findElement(By.id("whoamiBtn")).click();
    const whoamiResponseElem = await this.driver.findElement(By.id("whoamiResponse"));
    await this.driver.wait(until.elementTextContains(whoamiResponseElem, "-"), 6_000);
    return await whoamiResponseElem.getText();
  }
}

async function fillText(driver: ThenableWebDriver, id: string, text: string) {
  const elem = await driver.findElement(By.id(id));
  elem.clear();
  elem.sendKeys(text);
}
