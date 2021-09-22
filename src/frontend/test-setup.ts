import crypto from "@trust/webcrypto";
import textEncoding = require("text-encoding");
import * as ChildProc from "child_process";

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

let seleniumServerProc: ChildProc.ChildProcess;

beforeAll(async () => {
  console.log("starting selenium-standalone server...");
  seleniumServerProc = ChildProc.spawn("npx", ["selenium-standalone", "start"]);

  const promise = new Promise((resolve, reject) => {
    seleniumServerProc.stdout?.on("data", (data) => {
      console.log(`stdout: ${data}`);
      if (data.toString() === "Selenium started\n") {
        console.log("selenium-standalone started");
        resolve(true);
      }
    });

    seleniumServerProc.on("error", (err) => {
      console.error("Failed to start selenium-server: ", err);
      reject(err);
    });

    setTimeout(() => {
      reject("selenium-standalone server startup timeout");
    }, 30_000);
  });

  await promise;
});

afterAll(() => {
  console.log("stopping selenium-standalone server...");
  seleniumServerProc.kill();
});
