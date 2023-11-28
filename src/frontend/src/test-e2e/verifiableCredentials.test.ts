import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  runInBrowser,
  switchToPopup,
  waitToClose,
} from "./util";
import { IssuerAppView } from "./views";

import { II_URL, ISSUER_APP_URL } from "./constants";

test("Can add employee on issuer app", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const issuerAppView = new IssuerAppView(browser);
    await issuerAppView.open({ issuerAppUrl: ISSUER_APP_URL, iiUrl: II_URL });
    await issuerAppView.waitForDisplay();
    expect(await issuerAppView.isAuthenticated()).toBe(false);
    await issuerAppView.authenticate();
    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(browser);
    await waitToClose(browser);
    const principal = await issuerAppView.waitForAuthenticated();
    const msg = await issuerAppView.addEmployee();
    expect(msg).toContain("Added");
    expect(msg).toContain(principal);
  });
}, 300_000);
