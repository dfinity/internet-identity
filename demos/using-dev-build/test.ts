#!/usr/bin/env ts-node
/* This starts the test suite. The II canister id is read from dfx.
 * This expects the replica to be running, and expects the II canister to have
 * been deployed.
 */

import { execSync, spawn } from "child_process";

/**
 * Read a canister ID from dfx's local state
 */
export const readCanisterId = ({
  canisterName,
}: {
  canisterName: string;
}): string => {
  const command = `dfx canister id ${canisterName}`;
  try {
    const stdout = execSync(command);
    return stdout.toString().trim();
  } catch (e) {
    throw Error(
      `Could not get canister ID for '${canisterName}' with command '${command}', was the canister deployed? ${e}`
    );
  }
};

function getCanisterHost({ canisterName }: { canisterName: string }): string {
  const port = execSync("dfx info webserver-port");
  const canisterId = readCanisterId({ canisterName });
  return `http://${canisterId}.localhost:${port}`;
}

function main() {
  console.log("Starting tests");
  const wdio = spawn("npm", ["run", "wdio"], {
    env: {
      ...process.env,
      II_DAPP_URL: getCanisterHost({ canisterName: "internet_identity" }),
    },
  });

  wdio.stdout.on("data", (data) => {
    console.log(`wdio: ${data}`);
  });

  wdio.stderr.on("data", (data) => {
    console.log(`wdio: ${data}`);
  });

  wdio.on("exit", (code, signal) => {
    if (code !== null && code > 0) {
      // if code is set and non-zero, bubble up error from wdio tests
      throw new Error(`End-to-end tests returned with ${code}`);
    } else if (signal) {
      // otherwise, if the child was killed externally
      throw new Error(`End-to-end tests killed with signal ${signal}`);
    } else {
      console.log("End-to-end tests finished successfully");
    }
  });
}

main();
