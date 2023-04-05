#!/usr/bin/env ts-node
/* This starts the proxy and the test suite. The proxy configuration is read
 * from dfx.json and canister_ids.json. The proxy is shutdown after the tests
 * are run.
 * This expects the replica to be running, and expects the II canister to have
 * been deployed.
 */

import * as fs from "fs";
import { ChildProcess, spawn, execSync } from "child_process";

// In order to proxy the calls Node v18 requires 127.0.0.1 instead of localhost as string
const LOCALHOST = "127.0.0.1";

// Port and URL for II and the sign in app, then exported to the tests
const II_DAPP_PORT = 8086;
const II_DAPP_URL = `http://${LOCALHOST}:${II_DAPP_PORT}`;

const WEBAPP_PORT = 8087;
const WEBAPP_URL = `http://${LOCALHOST}:${WEBAPP_PORT}`;

type Arguments = { noRun: boolean };

/** Parse CLI arguments
 */
function parseArguments(): Arguments {
  if (![2, 3].includes(process.argv.length)) {
    throw new Error("Usage: test.ts [--no-run]");
  }

  if (process.argv.length === 3) {
    if (process.argv[2] !== "--no-run") {
      throw new Error("Usage: test.ts [--no-run]");
    }

    return { noRun: true };
  } else {
    return { noRun: false };
  }
}

type CanisterIDs = { webapp: string; internetIdentity: string };

/*
 * Read the values from dfx.json and canister_ids.json
 */
function parseCanisterIDs(): CanisterIDs {
  const CANISTER_IDS_PATH = `${__dirname}/.dfx/local/canister_ids.json`;
  try {
    const canister_ids = JSON.parse(fs.readFileSync(CANISTER_IDS_PATH, "utf8"));
    return {
      internetIdentity: canister_ids["internet_identity"].local,
      webapp: canister_ids["webapp"].local,
    };
  } catch (e) {
    console.log(
      `Could not read 'internet_identity' and 'webapp' local canister IDs from ${CANISTER_IDS_PATH}`
    );
    throw e;
  }
}

function getReplicaHost(): string {
  let port = execSync("dfx info webserver-port");
  return `http://${LOCALHOST}:${port}`;
}

/*
 * Start the proxy
 *
 * Any port would do here, it just needs to be the same the test runner uses,
 * hence we set it as the `II_DAPP_URL` environment variable which is read by
 * wdio.conf.js..
 */

function spawnProxy(
  canisterIds: CanisterIDs,
  replicaHost: string
): ChildProcess {
  return spawn("proxy", [
    "--replica-host",
    replicaHost,
    `${canisterIds.internetIdentity}:${II_DAPP_PORT}`,
    `${canisterIds.webapp}:${WEBAPP_PORT}`,
  ]);
}

function main() {
  // Read values and spawn proxy
  const canisterIds = parseCanisterIDs();
  console.log(
    `Using canister IDs: internet_identity: ${canisterIds.internetIdentity}, webapp: ${canisterIds.webapp}`
  );

  const replicaHost = getReplicaHost();
  console.log(`Using replica host: ${replicaHost}`);

  const proxy = spawnProxy(canisterIds, replicaHost);

  const runTests = !parseArguments().noRun;

  let testsStarted = false;
  let proxyOutput = "";

  proxy.stdout.on("data", (data) => {
    console.log(`proxy: ${data}`);

    if (!testsStarted) {
      // Wait for proxy to output "Proxy created" before we start the tests, otherwise the
      // tests may start before the proxy is running.
      proxyOutput = `${proxyOutput}${data}`;
      if (proxyOutput.includes("Proxy created")) {
        console.log("Proxy is up and running");
        testsStarted = true;

        // if runTests is true, then the tests are run, and then we return.
        // Otherwise, we simply leave the proxy running and wait to be killed by the user.
        if (runTests) {
          /*
           * Start the tests
           */
          console.log("Starting tests");
          const wdio = spawn("npm", ["run", "wdio"], {
            env: { ...process.env, II_DAPP_URL: II_DAPP_URL },
          });

          wdio.stdout.on("data", (data) => {
            console.log(`wdio: ${data}`);
          });

          wdio.stderr.on("data", (data) => {
            console.log(`wdio: ${data}`);
          });

          wdio.on("exit", (code, signal) => {
            console.log("Killing proxy");
            proxy.kill();
            if (code > 0) {
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
      }
    }
  });

  proxy.stderr.on("data", (data) => {
    console.log(`proxy: ${data}`);
  });

  proxy.on("exit", (code, signal) => {
    console.log(`proxy returned (return code: ${code}, signal: ${signal})`);
  });
}

main();
