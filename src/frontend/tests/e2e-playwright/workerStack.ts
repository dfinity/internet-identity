/**
 * Every Playwright worker runs against an e2e stack of its own, which
 * `scripts/dev-e2e-setup` sets up: an icp network, a dev server and the test
 * OpenID providers, on ports offset by the worker's parallel index. This module
 * derives the current worker's ports with the same layout as that script and
 * the `e2e-<i>` networks in `icp.yaml`.
 */

/** The stacks `scripts/dev-e2e-setup` sets up when `E2E_WORKERS` is unset. */
const DEFAULT_E2E_WORKERS = 4;

/** The number of Playwright workers: one per stack. */
export const E2E_WORKERS = Number(
  process.env.E2E_WORKERS ?? DEFAULT_E2E_WORKERS,
);

/**
 * The current worker's index, from 0 to `E2E_WORKERS - 1`. Playwright sets
 * `TEST_PARALLEL_INDEX` in every worker process; the runner process, which
 * launches no browser, has none and gets 0.
 */
const WORKER_INDEX = Number(process.env.TEST_PARALLEL_INDEX ?? 0);

/** Worker 0's dev server port; worker i's is this plus i. */
const BASE_DEV_SERVER_PORT = 5173;

/** Worker 0's gateway port; worker i's is this plus i. */
const BASE_GATEWAY_PORT = 8000;

/** The distance between the OpenID provider ports of consecutive workers. */
const OPENID_PORT_STRIDE = 100;

/** The port of the current worker's dev server. */
export const DEV_SERVER_PORT = BASE_DEV_SERVER_PORT + WORKER_INDEX;

/** The port of the current worker's local gateway. */
export const GATEWAY_PORT = BASE_GATEWAY_PORT + WORKER_INDEX;

/**
 * The current worker's port for the OpenID provider that worker 0 runs on
 * `basePort`.
 */
export const workerOpenIdPort = (basePort: number): number =>
  basePort + WORKER_INDEX * OPENID_PORT_STRIDE;
