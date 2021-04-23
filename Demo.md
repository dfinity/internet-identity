# Demo instructions

## Setup

These instructions start from a mostly clean slate. Apply common sense when following them in your development area.

* Delete your browser cache for `identity.localhost`
* Build the `icx-proxy`:
  - In a checkout of `dfinity/agent-rs`
  - switch to branch `next`.
  - cargo build --release

* Run `./dfx.sh cache delete`, just to be sure
* In a checkout of `idp-service`, `main` branch, reset the dfx state:
  ```
  rm -rf .dfx
  ```
* In a checkout of `dfinity-lab/open-chat`, switch to branch `joachim/idp-demo-proxy`.
  This configures the open chat frontend to use
  <http://identity.localhost/authorize.html> as the OAUTH endpoint.
* In a checkout of open chat, reset the dfx state:
  ```
  rm -rf .dfx
  ```
  (or less expensive, `rm -f .dfx/local/canister_ids.json .dfx/local/wallets.json`)
* In idp-service, run the replica. By virtue of using `dfx.sh`, this will pull
  in a replica with our auth changes (via https://github.com/dfinity/sdk/pull/1587)
  ```
  ../idp-service/dfx.sh start
  ```
  Note the port when it says
  ```
  replica(s): http://localhost:41549/
  ```

* In `idp-service` , build and deploy:
  ```
  npm ci
  ../idp-service/dfx.sh deploy --argument '(null)'
  ```
  Note the canister id

* In open chat, build and deploy:
  ```
  npm install
  ../idp-service/dfx.sh deploy
  ```

* In `agent-js` run
  ```
  sudo ./target/release/icx-proxy --address 127.0.0.1:80 --replica http://localhost:41549/ --dns-alias identity.localhost:rrkah-fqaaa-aaaaa-aaaaq-cai -v
  ```
  putting in the port of the replica, and the canister id of the identity server.

* Open open chat in your browser, by running this in `open-chat`
  ```
  google-chrome "http://$(dfx canister id website).localhost/"
  ```

You should be ready to rumble!

## Demo scene 1: OAUTH + registration

Showing registration.

1. On the open chat log in page, click the button.
2. Now you are at the IPD frontend. Click register
3. Follow the instructions:

   * tapping your yubikey to create an identity
   * tapping it again to register
   * observe your User Id

4. Now you should be forwarded back to Open Chat.
5. Point our your identity on Open Chat (TODO: Where? Didn't check)

## Demo scene 2: re-login

Showing re-login with fresh browser.

Clear all browser state, or maybe use a different browser (e.g. Firefox vs. Chrome)

1. On the open chat log in page, click the button.
2. Now you are at the IPD frontend. Click login with existing device.
3. Follow the instructions:

   * enter your User id
   * tap your yubikey to confirm its use

4. Now you should be forwarded back to Open Chat.
5. Point our your identity on Open Chat, and see that it is the same


## Demo scene 3: fresh device

(Requires two keys)

Use a different browser with fresh state, using two browsers to simulate two
different devices. Ideally use two yubikeys to demo.

1. On the open chat log in page, click the button.
2. Now you are at the IPD frontend. Click login with new device
3. Follow the instructions:

   * enter your User id
   * get the link
   * copy’n’paste that link in the other browser
   * there you are still logged in. you have to confirm that you want
     to add a device, give an alias, and tap your yubikey to confirm
   * back at the first browser reload (or maybe the page has been polling anyways)

4. Now you should be forwarded back to Open Chat.
5. Point our your identity on Open Chat, and see that it is _still_ the same
