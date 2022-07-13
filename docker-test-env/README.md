# Docker Compose Test Environment

Docker compose setup to run selenium tests against. The setup consists of the following components:
* dfx-docker container
  * Runs `dfx` with Internet Identity and the test-app (built from `demos/test-app`) deployed.
* nginx container
  * Required to terminate TLS.
  * Forwards requests to the `dfx-docker` container.
  * Translates domains `<canister_id>.ic0.app` to the corresponding `<canister_id>.localhost` domains.
  * Translates mapped domains (i.e. `identity.ic0.app`) to the corresponding `<canister_id>.localhost` domain.
* selenium container
  * Runs chrome browser.
  * Connects to nginx to access pages of the canister hosted in the `dfx-docker` container

When selenium tests are run, the tests are executed on the host machine natively and connect to the selenium container using the webdriver interface.

To run selenium tests, do the following:
1. Build Internet Identity by running `II_FETCH_ROOT_KEY=1 II_DUMMY_CAPTCHA=1 scripts/docker-build`.
2. Copy `internet_identity.wasm` to `docker-test-env/dfx-docker/canisters`.
3. Copy `src/internet_identity/internet_identity.did` to `docker-test-env/dfx-docker/canisters`.
4. Build the test app by running `demos/test-app/build.sh`.
5. Copy `test_app.wasm` to `docker-test-env/dfx-docker/canisters`.
6. Copy `demos/test-app/test_app.did` to `docker-test-env/dfx-docker/canisters`.
7. Run `docker compose build` from this folder to build the containers .
8. Run `docker compose up` from this folder to start the containers.
   1. Add `-d --wait` to run it in the background and `docker compose down` to stop it again.
9. Run `npm run test:e2e-desktop` or `npm run test:e2e-mobile` from the repository root to run the selenium tests.

**Note:** Steps 1 - 7 have to be only repeated if the canister code changes. Step 9 can be repeated as many times as required.

The following interesting interfaces are exposed by the docker compose setup:
* The replica is accessible on port `8000` (i.e. for canister calls with `dfx`).
* It is possible to connect to the selenium container to watch the tests being executed:
  1. Remove the `--headless` option in `src/frontend/src/test-e2e/util.ts:31`. Additionally, the window size can also be adjusted for a better viewing experience.
  2. Open `http://localhost:7900/` in the browser. The password is `secret`.
