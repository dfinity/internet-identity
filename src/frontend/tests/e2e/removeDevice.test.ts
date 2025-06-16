import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  removeVirtualAuthenticator,
  runInBrowser,
} from "./util";
import { AuthenticateView, MainView } from "./views";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
// TEMPORARILY REMOVE DEVICE_NAME2 TO SATISFY TS
import { DEVICE_NAME1, II_URL } from "./constants";

test("Removing current device logs user out", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const firstAuthenticator = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await removeVirtualAuthenticator(browser, firstAuthenticator);
    await addVirtualAuthenticator(browser);
    await FLOWS.addFidoDevice(browser);

    // Manage page
    await mainView.waitForDisplay();
    // Both devices have the same name.
    await mainView.waitForDeviceCount(DEVICE_NAME1, 2);

    // Remove the device used when registering.
    // Both devices have the same name.
    await mainView.remove(DEVICE_NAME1);

    // Verify the user is logged out and back to the home screen
    const welcomeView = new AuthenticateView(browser);
    await welcomeView.waitForDisplay();
  });
}, 300_000);

// The renaming is necessary because the first and second devices have the same name
// and the selector uses the name to find the device, which defaults to the first device.
// Unfortunately, we can't change the name of the device when adding it.
// This is something we could improve in the future.

// TEMPORARILY DEACTIVATED BECAUSE INTERFERES WITH BANNER

// test("User can register, add device, rename first device, remove the second device", async () => {
//   await runInBrowser(async (browser: WebdriverIO.Browser) => {
//     const firstAuthenticator = await addVirtualAuthenticator(browser);
//     await browser.url(II_URL);
//     await FLOWS.registerNewIdentityWelcomeView(browser);
//     const mainView = new MainView(browser);
//     await mainView.waitForDeviceDisplay(DEVICE_NAME1);
//     await removeVirtualAuthenticator(browser, firstAuthenticator);
//     await addVirtualAuthenticator(browser);
//     await FLOWS.addFidoDevice(browser);

//     // Manage page
//     await mainView.waitForDisplay();
//     // Both devices have the same name.
//     await mainView.waitForDeviceCount(DEVICE_NAME1, 2);

//     // Rename the device used when registering.
//     await mainView.rename(DEVICE_NAME1, DEVICE_NAME2);

//     // Remove the device added after registering.
//     await mainView.remove(DEVICE_NAME1);

//     // Verify the device count is back to 1
//     await mainView.waitForDeviceCount(DEVICE_NAME2, 1);
//   });
// }, 300_000);

test("User can add and remove a recovery device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const firstAuthenticator = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.expectRecoveryDevice(false);

    // Add a recovery device
    await removeVirtualAuthenticator(browser, firstAuthenticator);
    await addVirtualAuthenticator(browser);
    await FLOWS.addRecoveryMechanismDevice(browser);

    // Verify the recovery device is added
    await mainView.expectRecoveryDevice(true);

    // Remove the recovery device
    await mainView.removeRecovery();

    // Verify the recovery device is removed
    await mainView.expectRecoveryDevice(false);
  });
}, 300_000);
