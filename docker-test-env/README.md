# Docker Compose Test Environment

Docker compose setup to run selenium tests with. The setup consists of the following components:
* nginx container
  * Required to terminate TLS.
  * Forwards requests to the test app to `dfx` running on host.
  * Forwards requests to the internet identity to the dev server running on host.
  * Translates domains `<canister_id>.ic0.app` to the corresponding `<canister_id>.localhost` domains.
  * Translates mapped domains (i.e. `identity.internetcomputer.org`) to the corresponding `<canister_id>.localhost` domain.
* selenium container
  * Runs chromium browser.
  * Connects to nginx to access pages of the canister hosted on `dfx` or the dev server

When selenium tests are run, the tests are executed on the host machine natively and connect to the selenium container using the webdriver interface.

To run selenium tests, do the following:
1. Run `dfx start` from the repository root.
   1. The `--background` flag can be added to run in the background.
2. Run `II_FETCH_ROOT_KEY=1 II_DUMMY_CAPTCHA=1 dfx deploy --no-wallet`.
3. Switch to the `demos/test-app` directory and run `dfx deploy --no-wallet`.
4. Run `npm run dev` from the repository root.
5. Run `scripts/start-selenium-env` from the repository root.
   1. The docker compose setup can be shut down by running `docker compose down` in the `docker-test-env` directory.
   2. The docker compose setup has to be restarted only if the canister ids change. Additional `dfx deploy` commands or changes to the front-end of II do not require a restart of the docker compose project.
6. Run `npm run test:e2e-desktop` or `npm run test:e2e-mobile` from the repository root to run the selenium tests.

It is possible to connect to the selenium container to watch the tests being executed by opening `http://localhost:7900/` in the browser. The password is `secret`.
