# @dfinity/auth-client

Simple interface to get your web application authenticated with the Internet Identity Service

Visit the [Dfinity Forum](https://forum.dfinity.org/) and [SDK Documentation](https://sdk.dfinity.org/docs/index.html) for more information and support building on the Internet Computer.

Additional API Documentation can be found [here](https://peacock.dev/auth-client-docs).

---

## Installation

Using AuthClient:

```
npm i --save @dfinity/auth-client
```

### In the browser:

```
import * as auth from "@dfinity/auth-client";
```

or using individual exports:

```
import { AuthClient } from "@dfinity/auth-client";
```

To get started with auth client, run

```js
const authClient = await AuthClient.create();
```

The authClient can log in with

```js
authClient.loginWithRedirect();
```

It handles redirects, saves your delegation to localStorage, and then sets you up with an identity.

```js
if (location.hash.substring(1).startsWith('access_token')) {
  const identity = await authClient.handleRedirectCallback();
}
```

Then, you can use that identity to make authenticated calls using the `@dfinity/agent` `Actor`.

```js
const actor = Actor.createActor(idlFactory, {
  agent: new HttpAgent({
    host: hostUrlEl.value,
    identity,
  }),
  canisterId,
});
```

Note: depends on [@dfinity/agent](https://www.npmjs.com/package/@dfinity/agent), [@dfinity/authentication](https://www.npmjs.com/package/@dfinity/agent), and
[@dfinity/identity](https://www.npmjs.com/package/@dfinity/identity).
