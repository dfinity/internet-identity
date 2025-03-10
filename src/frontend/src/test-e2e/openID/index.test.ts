// import { II_URL, TEST_APP_NICE_URL } from "../constants";
// import { FLOWS } from "../flows";
// import {
//   addVirtualAuthenticator,
//   runInBrowser,
//   setOpenIdFeatureFlag,
// } from "../util";
// import { AuthenticateView, DemoAppView, MainView } from "../views";

// // Mock JWT token for testing
// const MOCK_JWT =
//   "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJzdWIiOiIxMjM0NTY3ODkwIiwiYXVkIjoiY2xpZW50X2lkIiwiZW1haWwiOiJ0ZXN0QGV4YW1wbGUuY29tIiwibm9uY2UiOiJtb2NrX25vbmNlIn0.signature";

// test("Register, link Google account, logout, and login with Google", async () => {
//   await runInBrowser(async (browser: WebdriverIO.Browser) => {
//     // Mock window.open to intercept Google OAuth popup
//     await browser.execute(() => {
//       const originalOpen = window.open;
//       window.open = function mockOpen(
//         url?: string | URL,
//         target?: string,
//         features?: string
//       ): Window | null {
//         if (typeof url === "string" && url.includes("accounts.google.com")) {
//           // Simulate successful OAuth flow by posting message back
//           setTimeout(() => {
//             window.postMessage(
//               {
//                 data: MOCK_JWT,
//                 origin: window.location.origin,
//               },
//               window.location.origin
//             );
//           }, 100);
//           return null;
//         }
//         return originalOpen(url, target, features);
//       };
//     });

//     // Enable OpenID feature flag
//     await setOpenIdFeatureFlag(browser, true);

//     // First register a new identity
//     await browser.url(II_URL);
//     await addVirtualAuthenticator(browser);
//     const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);

//     // Get to the main view
//     const mainView = new MainView(browser);
//     await mainView.waitForDeviceDisplay("Device 1");

//     // Add Google OpenID account
//     await mainView.addOpenIdCredential();

//     // The mock will automatically handle the Google OAuth flow
//     // Wait for the credential to be added
//     await browser.waitUntil(
//       async () => {
//         const linkedAccounts = await mainView.getLinkedAccounts();
//         return linkedAccounts.length > 0;
//       },
//       { timeout: 5000 }
//     );

//     // Verify Google account is linked
//     const linkedAccounts = await mainView.getLinkedAccounts();
//     expect(linkedAccounts[0].iss).toBe("https://accounts.google.com");
//     expect(linkedAccounts[0].sub).toBe("1234567890");

//     // Logout
//     await mainView.logout();

//     // Try to login with Google
//     const authenticateView = new AuthenticateView(browser);
//     await authenticateView.waitForDisplay();
//     await authenticateView.loginWithGoogle();

//     // The mock will automatically handle the Google OAuth flow
//     // Verify successful login
//     await mainView.waitForDeviceDisplay("Device 1");

//     // Verify the authentication method
//     const demoAppView = new DemoAppView(browser);
//     await demoAppView.open(TEST_APP_NICE_URL, II_URL);
//     await demoAppView.waitForDisplay();
//     expect(await demoAppView.getAuthnMethod()).toBe("openid");
//   });
// }, 300_000);
