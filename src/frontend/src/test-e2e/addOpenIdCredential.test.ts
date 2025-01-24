import { PocketIc, PocketIcServer } from "@hadronous/pic";
import path from "path";
import { DEVICE_NAME1 } from "./constants";
import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  mockFedCM,
  runInBrowser,
  setOpenIdFeatureFlag,
} from "./util";
import { MainView } from "./views";

const TEST_GOOGLE_OPENID_TOKEN =
  "eyJhbGciOiJSUzI1NiIsImtpZCI6ImRkMTI1ZDVmNDYyZmJjNjAxNGFlZGFiODFkZGYzYmNlZGFiNzA4NDciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI0NTQzMTk5NDYxOS1jYmJmZ3RuN28wcHAwZHBmY2cybDY2YmM0cmNnN3FidS5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsImF1ZCI6IjQ1NDMxOTk0NjE5LWNiYmZndG43bzBwcDBkcGZjZzJsNjZiYzRyY2c3cWJ1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTE1MTYwNzE2MzM4ODEzMDA2OTAyIiwiaGQiOiJkZmluaXR5Lm9yZyIsImVtYWlsIjoidGhvbWFzLmdsYWRkaW5lc0BkZmluaXR5Lm9yZyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJub25jZSI6ImV0aURhTEdjUmRtNS1yY3FlMFpRVWVNZ3BmcDR2OVRPT1lVUGJoUng3bkkiLCJuYmYiOjE3MzY3OTM4MDIsIm5hbWUiOiJUaG9tYXMgR2xhZGRpbmVzIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0lTTWxja0M1RjZxaGlOWnpfREZtWGp5OTY4LXlPaEhPTjR4TGhRdXVNSDNuQlBXQT1zOTYtYyIsImdpdmVuX25hbWUiOiJUaG9tYXMiLCJmYW1pbHlfbmFtZSI6IkdsYWRkaW5lcyIsImlhdCI6MTczNjc5NDEwMiwiZXhwIjoxNzM2Nzk3NzAyLCJqdGkiOiIwMWM1NmYyMGM1MzFkNDhhYjU0ZDMwY2I4ZmRiNzU0MmM0ZjdmNjg4In0.f47b0HNskm-85sT5XtoRzORnfobK2nzVFG8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww6Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";

test("Add OpenID credential", async () => {
  // Deploy canister with pocket ic
  // const canisterId = Principal.fromUint8Array(new Uint8Array([0]));
  const picServer = await PocketIcServer.start();
  // const pic = await PocketIc.create(picServer.getUrl());
  const pic = await PocketIc.create(picServer.getUrl());
  const canisterId = await pic.createCanister();
  console.log("canisterId", canisterId.toText());
  await pic.installCode({
    canisterId,
    wasm: path.join(__dirname, "../../../../internet_identity.wasm.gz"),
  });
  const url = `http://${canisterId.toText()}.localhost:4943`;
  console.log("url", url);

  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    await browser.url(url);
    await setOpenIdFeatureFlag(browser, true);
    await mockFedCM(browser, TEST_GOOGLE_OPENID_TOKEN);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.addOpenIdCredential();
    await browser.pause(1000000);
  });
}, 300_000);
