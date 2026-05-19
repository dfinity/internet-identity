#!/usr/bin/env node
// File watcher spawned by scripts/dev-e2e. Watches the canister source dirs
// and the frontend assets dir; on change, rebuilds and upgrades the affected
// canister using the same init-args files that scripts/dev-e2e-setup wrote.

import { watch, appendFileSync } from "node:fs";
import { spawn } from "node:child_process";

const II_ARGS_FILE = process.env.DEV_E2E_II_ARGS_FILE;
const FE_ARGS_FILE = process.env.DEV_E2E_FE_ARGS_FILE;
const LOG_FILE = process.env.DEV_E2E_LOG_FILE;

if (!II_ARGS_FILE || !FE_ARGS_FILE) {
  console.error(
    "[watcher] DEV_E2E_II_ARGS_FILE and DEV_E2E_FE_ARGS_FILE must be set",
  );
  process.exit(1);
}

const targets = [
  {
    name: "internet_identity",
    dirs: ["src/internet_identity"],
    buildFlag: "--internet-identity",
    wasm: "internet_identity.wasm.gz",
    argsFile: II_ARGS_FILE,
  },
  {
    name: "internet_identity_frontend",
    // FE canister bundles src/frontend assets via `npm run build`, so a UI
    // change should redeploy the FE canister too.
    dirs: ["src/internet_identity_frontend", "src/frontend"],
    buildFlag: "--frontend",
    wasm: "internet_identity_frontend.wasm.gz",
    argsFile: FE_ARGS_FILE,
  },
];

function log(msg) {
  const line = `[watcher] ${msg}\n`;
  process.stdout.write(line);
  if (LOG_FILE) {
    try {
      appendFileSync(LOG_FILE, line);
    } catch {}
  }
}

function run(cmd, args, { env } = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(cmd, args, {
      stdio: "inherit",
      env: { ...process.env, ...env },
    });
    child.on("exit", (code) =>
      code === 0
        ? resolve()
        : reject(new Error(`${cmd} ${args.join(" ")} exited with ${code}`)),
    );
  });
}

async function rebuild(target) {
  log(`change detected — rebuilding ${target.name}`);
  try {
    await run("./scripts/build", [target.buildFlag], {
      env: { II_FETCH_ROOT_KEY: "1" },
    });
    log(`upgrading ${target.name}`);
    await run("icp", [
      "canister",
      "install",
      target.name,
      "-y",
      "--mode",
      "upgrade",
      "--wasm",
      target.wasm,
      "--args-file",
      target.argsFile,
    ]);
    log(`${target.name} upgraded`);
  } catch (err) {
    log(`rebuild failed: ${err.message}`);
  }
}

// Per-target debounce: editors save in bursts (Vim's atomic write, JetBrains'
// safe save) so a single user save produces many fs events. Per-target queue
// of at most one pending re-run prevents overlapping rebuilds.
const timers = new Map();
const inflight = new Map();

function schedule(target) {
  clearTimeout(timers.get(target.name));
  timers.set(
    target.name,
    setTimeout(async () => {
      if (inflight.get(target.name)) {
        inflight.set(target.name, "queued");
        return;
      }
      inflight.set(target.name, "running");
      await rebuild(target);
      const state = inflight.get(target.name);
      inflight.set(target.name, null);
      if (state === "queued") schedule(target);
    }, 400),
  );
}

function isWatchedFile(filename) {
  return /\.(rs|toml|did|ts|svelte|json|css|html|po)$/.test(filename);
}

for (const target of targets) {
  for (const dir of target.dirs) {
    try {
      watch(dir, { recursive: true }, (_event, filename) => {
        if (!filename || !isWatchedFile(filename)) return;
        schedule(target);
      });
      log(`watching ${dir} → ${target.name}`);
    } catch (err) {
      log(`failed to watch ${dir}: ${err.message}`);
    }
  }
}

process.on("SIGINT", () => process.exit(0));
process.on("SIGTERM", () => process.exit(0));
