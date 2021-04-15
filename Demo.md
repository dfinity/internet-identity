# Demo instructions

## Setup

These instructions start from a mostly clean slate. Apply common sense when following them in your development area.

1. Run `./dfx.sh cache delete`, just to be sure
2. In a checkout of `idp-service`, `main` branch, reset the dfx state:
   ```
   rm -rf .dfx
   ```
3. In a checkout of `dfinity-lab/open-chat`, switch to branch `joachim/idp`.
   This configures the open chat frontend to use
   <http://localhost:8080/authorize.html> as the OAUTH endpoint.
4. In a checkout of open chat, reset the dfx state:
   ```
   rm -rf .dfx
   ```
5. In open chat, run the replica. By virtue of using `dfx.sh`, this will pull
   in a replica with our auth changes (via https://github.com/dfinity/sdk/pull/1587)
   ```
   ../idp-service/dfx.sh start --background
   ```
6. In open chat, build and deploy:
   ```
   npm install
   ../idp-service/dfx.sh deploy
   ```
7. Open open chat in your browser
   ```
   firefox "http://localhost:8000?canisterId=$(dfx canister id website)"
   ```
8. In `idp-service` idenit chat, build and deploy:
   ```
   npm install
   ../idp-service/dfx.sh deploy
   ```
9. In `idp-service` idenit chat, run the development server at port 8080:
   ```
   start
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


