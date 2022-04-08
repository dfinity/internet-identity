#!/usr/bin/env node
/* This starts the proxy and the test suite. The proxy configuration is read
 * from dfx.json and canister_ids.json. The proxy is shutdown after the tests
 * are run.
 * This expects the replica to be running, and expects the II canister to have
 * been deployed.
 */

"use strict";

const fs = require("fs");

let runTests = true;

if (!(process.argv.length == 2 || process.argv.length == 3)) {
  throw new Error("Usage: test.js [--no-run]");
}

if (process.argv.length == 3) {
  if (process.argv[2] !== "--no-run") {
    throw new Error("Usage: test.js [--no-run]");
  }
  runTests = false;
}

/*
 * Read the values from dfx.json and canister_ids.json
 */
const CANISTER_IDS_PATH = `${__dirname}/.dfx/local/canister_ids.json`;
let ii_canister_id;
let webapp_canister_id;
try {
  const canister_ids = JSON.parse(fs.readFileSync(CANISTER_IDS_PATH, "utf8"));
  ii_canister_id = canister_ids["internet_identity"].local;
  webapp_canister_id = canister_ids["webapp"].local;
} catch (e) {
  console.log(
    `Could not read 'internet_identity' and 'webapp' local canister IDs from ${CANISTER_IDS_PATH}`
  );
  throw e;
}

console.log(
  `Using canister IDs: internet_identity: ${ii_canister_id}, webapp: ${webapp_canister_id}`
);

const DFX_JSON_PATH = `${__dirname}/dfx.json`;
let replica_host;
try {
  const dfx_json = JSON.parse(fs.readFileSync(DFX_JSON_PATH, "utf8"));
  replica_host = dfx_json.networks.local.bind;
  if (!replica_host.startsWith("http://")) {
    replica_host = `http://${replica_host}`;
  }
} catch (e) {
  console.log(`Could not read replica host from ${DFX_JSON_PATH}`);
  throw e;
}

console.log(`Using replica host: ${replica_host}`);

/*
 * Start the proxy
 *
 * Any port would do here, it just needs to be the same the test runner uses,
 * hence we set it as the `II_DAPP_URL` environment variable which is read by
 * wdio.conf.js..
 */
const II_DAPP_PORT = 8086;
const II_DAPP_URL = `http://localhost:${II_DAPP_PORT}`;
//
const WEBAPP_PORT = 8087;
const WEBAPP_URL = `http://localhost:${WEBAPP_PORT}`;

const child_process = require("child_process");

const proxy = child_process.spawn("proxy", [
  "--replica-host",
  replica_host,
  `${ii_canister_id}:${II_DAPP_PORT}`,
  `${webapp_canister_id}:${WEBAPP_PORT}`,
]);

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
        const wdio = child_process.spawn("npm", ["run", "wdio"], {
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
