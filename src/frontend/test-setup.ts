import crypto from "@trust/webcrypto";
import textEncoding = require("text-encoding");
import ChildProcess = require("child_process");

declare global {
  // eslint-disable-next-line @typescript-eslint/no-namespace
  namespace WebdriverIO {
    interface Browser {
      addVirtualWebAuth: (
        protocol: string,
        transport: string,
        hasResidentKey: boolean,
        isUserConsenting: boolean
      ) => Promise<string>;
      removeVirtualWebAuth: (authenticatorId: string) => Promise<void>;
    }
  }
}

global.crypto = crypto;
global.TextEncoder = textEncoding.TextEncoder;

console.log("starting selenium-standalone server...");
const seleniumServerProc = ChildProcess.spawn("selenium-standalone", ["start"]);

afterAll(() => {
  console.log("stopping selenium-standalone server...");
  seleniumServerProc.kill();
});
