# The Internet Identity Specification

## Introduction

This document describes and specifies Internet Identity from various angles and at various levels of abstraction, namely:

-   High level goals, requirements and use cases

-   Overview of the security and identity machinery, including the interplay of identities, keys, and delegations

-   Interface as used by client applications frontends, i.e., our [client authentication protocol](#client-authentication-protocol)

-   The interface of the Internet Identity Service *backend*, i.e., describing its contract at the Candid layer, as used by its frontend

-   Important implementation notes about the Internet Identity Service backend

-   Internal implementation notes about the Internet Identity Service frontend

The Internet Identity Service consists of

-   its backend, a canister on the IC. More precisely, a canister on a dedicated subnet with a *well-known* canister id, and

-   its frontend, a web application served by the backend canister.

Similarly, the client applications consist of a frontend (served by a canister) and (typically) one or more backend canisters. Only the frontend interacts with the Internet Identity Service directly (via the [client authentication protocol](#client-authentication-protocol) described below).

## Goals, requirements and use cases

The Internet Identity service allows users to

-   maintain identities on the Internet Computer

-   log in with these identities using one out of a set of security devices

-   manage their set of security devices

Some functional requirements are

-   users have separate identities (or "pseudonyms") per client application (more precisely, per client application frontend "hostname", though see [Alternative Frontend Origins](#alternative-frontend-origins) for caveat about `.raw` domains)

-   these identities are stable, i.e., do not depend on a user's security devices

-   the client frontends interact with any canister on the Internet Computer under the user's identity with that frontend

-   users do not need ever to remember secret information (but possibly per-user non-secret information)

-   a security device does not need to be manually touched upon every interaction with a client application; a login is valid for a certain amount of time per identity

Some security requirements are

-   The separate identities of a single user cannot be related merely based on their public key or principal ids, to impede user tracking.

-   The security of the identities does not depend on the privacy of data stored on canisters, or transmitted to and from canisters. In particular, the delegations handed out by the backend canister must not be sensitive information.

-   (many more, of course; apply common sense)

Some noteworthy security assumptions are:

-   The delivery of frontend applications is secure. In particular, a user accessing the Internet Identity Service Frontend through a TLS-secured HTTP connection cannot be tricked into running another web application.

:::note
Just for background: At launch this meant we relied on the trustworthiness of the boundary nodes as well as the replica the boundary nodes happens to fetch the assets from. After launch, certification of our HTTP Gateway protocol and trustworthy client-side code (browser extensions, proxies, etc.) have improved this situation.
:::

-   The security devices only allow the use of their keys from the same web application that created the key (in our case, the Internet Identity Service Frontend).

-   The user's browser is trustworthy, `postMessage` communication between different origins is authentic.

-   For user privacy, we also assume the Internet Identity Service backend can keep a secret (but since data is replicated, we do not rely on this assumption for other security properties).

## Identity design and data model


The Internet Computer serves this frontend under hostnames `https://identity.ic0.app` (official) and `https://identity.internetcomputer.org` (experimental).

The canister maintains a salt (in the following the `salt`), a 32 byte long blob that is obtained via the Internet Computer's source of secure randomness.


:::note
Due to replication of data in canisters, the salt should not be considered secret against a determined attacker. However, the canister will not reveal the salt directly and to the extent it is unknown to an attacker it helps maintain privacy of user identities.
:::

A user account is identified by a unique *Identity Anchor*, a smallish natural number chosen by the canister.

A client application frontend is identified by its hostname (e.g., `abcde-efg.ic0.app`, `nice-name.ic0.app`, `non-ic-application.com`). Frontend application can be served by canisters or by websites that are not hosted on the Internet Computer.

A user has a separate *user identity* for each client application frontend (i.e., per hostname). This identity is a [*self-authenticating id*](https://internetcomputer.org/docs/current/references/ic-interface-spec#id-classes) of the [DER encoded canister signature public key](https://internetcomputer.org/docs/current/references/ic-interface-spec/#canister-signatures) which has the form

    user_id = SHA-224(DER encoded public key) · 0x02` (29 bytes)

and the `BIT STRING` field of the DER encoded public key has the form  
  
    bit_string = |ii_canister_id| · ii_canister_id · seed

where the `seed` is derived as follows 

    seed = H(|salt| · salt · |user_number| · user_number · |frontend_host| · frontend_host)

where `H` is SHA-256, `·` is concatenation, `|…|` is a single byte representing the length of `…` in bytes, `user_number` is the ASCII-encoding of the Identity Anchor as a decimal number, and `frontend_host` is the ASCII-encoding of the client application frontend's hostname (at most 255 bytes).

:::note
A `frontend_host` of the form `<canister id>.icp0.io` will be rewritten to `<canister id>.ic0.app` before being used in the seed. This ensures transparent pseudonym transfer between apps hosted on `ic0.app` and `icp0.io` domains.
:::

The Internet Identity Service Backend stores the following data in user accounts, indexed by the respective Identity Anchor:

-   a set of *device information*, consisting of

    -   the device's public key (DER-encoded)

    -   a device *alias*, chosen by the user to recognize the device

    -   an optional *credential id*, which is necessary for WebAuthn authentication

When a client application frontend wants to authenticate as a user, it uses a *session key* (e.g., Ed25519 or ECDSA), and by way of the authentication flow (details below) obtains a [*delegation chain*](https://internetcomputer.org/docs/current/references/ic-interface-spec#authentication) that allows the session key to sign for the user's main identity.

The delegation chain consists of one delegation, called the *client delegation*. It delegates from the user identity (for the given client application frontend) to the session key. This delegation is created by the Internet Identity Service Canister, and signed using a [canister signature](https://hydra.dfinity.systems/latest/dfinity-ci-build/ic-ref.pr-319/interface-spec/1/index.html#canister-signatures). This delegation is unscoped (valid for all canisters) and has a maximum lifetime of 30 days, with a default of 30 minutes.

The Internet Identity Service Frontend also manages an *identity frontend delegation*, delegating from the security device's public key to a session key managed by this frontend, so that it can interact with the backend without having to invoke the security device for each signature.

## Client authentication protocol

This section describes the Internet Identity Service from the point of view of a client application frontend.

1.  The client application frontend creates a session key pair (e.g., Ed25519).

2.  It installs a `message` event handler on its own `window`.

3.  It loads the url `https://identity.ic0.app/#authorize` in a separate tab. Let `identityWindow` be the `Window` object returned from this.

4.  In the `identityWindow`, the user logs in, and the `identityWindow` invokes

        window.opener.postMessage(msg, "*")

    where `msg` is

        interface InternetIdentityReady {
          kind: "authorize-ready"
        }

5.  The client application, after receiving the `InternetIdentityReady`, invokes

        identityWindow.postMessage(msg, "https://identity.ic0.app")

    where `msg` is a value of type

        interface InternetIdentityAuthRequest {
          kind: "authorize-client";
          sessionPublicKey: Uint8Array;
          maxTimeToLive?: bigint;
          derivationOrigin?: string;
        }

    where

    -   the `sessionPublicKey` contains the public key of the session key pair.

    -   the `maxTimeToLive`, if present, indicates the desired time span (in nanoseconds) until the requested delegation should expire. The Identity Provider frontend is free to set an earlier expiry time, but should not create a one larger.

    -   the `derivationOrigin`, if present, indicates an origin that should be used for principal derivation instead of the client origin. Values must match the following regular expression: `^https:\/\/[\w-]+(\.raw)?\.(ic0\.app|icp0\.io)$`. Internet Identity will only accept values that are also listed in the HTTP resource `https://<canister_id>.ic0.app/.well-known/ii-alternative-origins` of the corresponding canister (see [Alternative Frontend Origins](#alternative-frontend-origins)).


6.  Now the client application window expects a message back, with data `event`.

7.  If `event.origin` is not either `"https://identity.ic0.app"` or `"https://identity.internetcomputer.org"` (depending on which endpoint you are using), ignore this message.

8.  The `event.data` value is a JS object with the following type:

        interface InternetIdentityAuthResponse {
          kind: "authorize-client-success";
          delegations: [{
            delegation: {
              pubkey: Uint8Array;
              expiration: bigint;
              targets?: Principal[];
            };
            signature: Uint8Array;
          }];
          userPublicKey: Uint8Array;
        }

    where the `userPublicKey` is the user's Identity on the given frontend and `delegations` corresponds to the CBOR-encoded delegation chain as used for [*authentication on the IC*](https://internetcomputer.org/docs/current/references/ic-interface-spec#authentication).

9.  It could also receive a failure message of the following type

        interface InternetIdentityAuthResponse {
          kind: "authorize-client-failure";
          text: string;
        }

    The client application frontend needs to be able to detect when any of the delegations in the chain has expired, and re-authorize the user in that case.

The [`@dfinity/auth-client`](https://www.npmjs.com/package/@dfinity/auth-client) NPM package provides helpful functionality here.

The client application frontend should support delegation chains of length more than one, and delegations with `targets`, even if the present version of this spec does not use them, to be compatible with possible future versions.

:::note
The Internet Identity frontend will use `event.origin` as the "Frontend URL" to base the user identity on. This includes protocol, full hostname and port. This means


-   Changing protocol, hostname (including subdomains) or port will invalidate all user identities.
    - However, multiple different frontend URLs can be mapped back to the canonical frontend URL, see [Alternative Frontend Origins](#alternative-frontend-origins).
    - Frontend URLs on `icp0.io` are mapped to `ic0.app` automatically, see [Identity design and data model](#identity-design-and-data-model).

-   The frontend application must never allow any untrusted JavaScript code to be executed, on any page on that hostname. Be careful when implementing a JavaScript playground on the Internet Computer.
:::

## Alternative Frontend Origins


To allow flexibility regarding the canister frontend URL, the client may choose to provide the canonical canister frontend URL (`https://<canister_id>.ic0.app` or `https://<canister_id>.raw.ic0.app`) as the `derivationOrigin` (see [Client authentication protocol](#client-authentication-protocol)). This means that Internet Identity will issue the same principals to the frontend (which uses a different origin) as it would if it were using one of the canonical URLs.

:::note
This feature is also available for `https://<canister id>.icp0.io` (resp. `https://<canister id>.raw.icp0.io`).
:::

:::caution
This feature is intended to allow more flexibility with respect to the origins of a _single_ service. Do _not_ use this feature to allow _third party_ services to use the same principals. Only add origins you fully control to `/.well-known/ii-alternative-origins` and never set origins you do not control as `derivationOrigin`!
:::

:::note
`https://<canister_id>.ic0.app` and `https://<canister_id>.raw.ic0.app` do _not_ issue the same principals by default . However, this feature can also be used to map `https://<canister_id>.raw.ic0.app` to `https://<canister_id>.ic0.app` principals or vice versa.
:::

:::note
In general, Internet Identity only allows alternative origins of the form `<canister id>.ic0.app` or `<canister id>.icp0.io`. There is one exception: `nns.ic0.app`, which is treated as `<nns-dapp canister id>.icp0.io`.
:::

In order for Internet Identity to accept the `derivationOrigin` the corresponding canister must list the frontend origin in the JSON object served on the URL `https://<canister_id>.ic0.app/.well-known/ii-alternative-origins` (i.e. the canister _must_ implement the `http_request` query call as specified [here](https://github.com/dfinity/interface-spec/blob/master/spec/index.adoc#the-http-gateway-protocol)).


### JSON Schema {#alternative-frontend-origins-schema}


``` json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "II Alternative Origins Principal Derivation Origins",
  "description": "An object containing the alternative frontend origins of the given canister, which are allowed to use a canonical canister URL (https://<canister_id>.ic0.app or https://<canister_id>.raw.ic0.app) for principal derivation.",
  "type": "object",
  "properties": {
    "alternativeOrigins": {
      "description": "List of allowed alternative frontend origins",
      "type": "array",
      "items": {
        "type": "string"
      },
      "minItems": 0,
      "maxItems": 10,
      "uniqueItems": true
    }
  },
  "required": [ "alternativeOrigins" ]
}
```



### Example
``` json
{
  "alternativeOrigins": [
    "https://alternative-1.com",
    "https://www.nice-frontend-name.org"
  ]
}
```

:::note
The path `/.well-known/ii-alternative-origins` will always be requested using the non-raw `https://<canister_id>.ic0.app` domain (even if the `derivationOrigin` uses a `.raw`) and _must_ be delivered as a certified asset. Requests to `/.well-known/ii-alternative-origins` _must_ be answered with a `200` HTTP status code. More specifically Internet Identity _will not_ follow redirects and fail with an error instead. These measures are required in order to prevent malicious boundary nodes or replicas from tampering with `ii-alternative-origins`.
:::

:::note
To prevent misuse of this feature, the number of alternative origins _must not_ be greater than 10.
:::

:::note
In order to allow Internet Identity to read the path `/.well-known/ii-alternative-origins`, the CORS response header [`Access-Control-Allow-Origin`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin) must be set and allow the Internet Identity origin `https://identity.ic0.app`.
:::

## The Internet Identity Service Backend interface

This section describes the interface that the backend canister provides.

This interface is currently only used by its own frontend. This tight coupling means that this interface may change, even in incompatible ways. We therefore do not have to apply Candid best practices for backward-compatibility (such as using records for arguments and results).

The summary is given by the Candid interface:

<!-- The '_attachments/` bit is an implementation detail of the docusaurus deploy -->

``` candid name=ii-interface file=_attachments/internet_identity.did
```


The `init_salt` method is mostly internal, see [Salt](#salt).

### The `register` and `create_challenge` methods

The `register` method is used to create a new user. The Internet Identity Service backend creates a *fresh* Identity Anchor, creates the account record, and adds the given device as the first device.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from the given `DeviceKey`.

In order to protect the Internet Computer from too many "free" update calls, and to protect the Internet Identity Service from too many user registrations, this call is protected using a CAPTCHA challenge. The `register` call can only succeed if the `ChallengeResult` contains a `key` for a challenge that was created with `create_challenge` (see below) in the last 5 minutes *and* if the `chars` match the characters that the Internet Identity Service has stored internally for that `key`.

### The `add` method

The `add` method appends a new device to the given user's record.

The Internet Identity Service backend rejects the call if the user already has a device on record with the given public key.

This may also fail (with a *reject*) if the user is registering too many devices.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

### The `remove` method

The `remove` method removes a device, identified by its public key, from the list of devices a user has.

It is allowed to remove the key that is used to sign this request. This can be useful for a panic button functionality.

It is allowed to remove the last key, to completely disable a user. The canister may forget that user completely then, assuming the Identity Anchor generation algorithm prevents new users from getting the same Identity Anchor.

It is the responsibility of the frontend UI to protect the user from doing these things accidentally.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

### The `enter_device_registration_mode` method

Enables device registration mode for the given identity anchor. When device registration mode is active, new devices can be added using `add_tentative_device` and `verify_tentative_device`. Device registration mode stays active for at most 15 minutes or until the flow is either completed or aborted.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

### The `exit_device_registration_mode` method

Exits device registration mode immediately. Any non verified tentative devices are discarded.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

### The `add_tentative_device` method

Tentatively adds a new device to the supplied identity anchor and returns a verification code. This code has to be used with the `verify_tentative_device` method to verify this device. If the flow is aborted or not completed within 15 minutes, the tentative device is discarded.

Tentatively added devices cannot be used to login into the management view or authorize authentications for other dApps.

**Authorization**: Anyone can call this

### The `verify_tentative_device` method

For an anchor in device registration mode: if called with a valid verification code, adds the tentative device as a regular device to the anchor and exits registration mode. The registration flow is aborted if this method is called five times with invalid verification codes.

Returns an error if called for a device not in registration mode.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

### The `lookup` query method

Fetches all device data associated with a user.

**Authorization**: Anyone can call this

### The `get_anchor_info` method

Fetches all data associated with an anchor including registration mode and tentatively registered devices.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

### The `get_principal` query method

Fetches the principal for a given user and front end.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

### The `prepare_delegation` method

The `prepare_delegation` method causes the Internet Identity Service backend to prepare a delegation from the user identity associated with the given Identity Anchor and Client Application Frontend Hostname to the given session key.

This method returns the user's identity that's associated with the given Client Application Frontend Hostname. By returning this here, and not in the less secure `get_delegation` query, we prevent attacks that trick the user into using a wrong identity.

The expiration timestamp is determined by the backend, but no more than `maxTimeToLive` (if present) nanoseconds in the future.

The method returns the expiration timestamp of the delegation. This is returned purely so that the client can feed it back to the backend in `get_delegation`.

The actual delegation can be fetched using `get_delegation` immediately afterwards.

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

### The `get_delegation` query method

For a certain amount of time after a call to `prepare_delegation`, a query call to `get_delegation` with the same arguments, plus the timestamp returned from `prepare_delegation`, actually fetches the delegation.

Together with the `UserKey` returned by `prepare_delegation`, the result of this method is used by the Frontend to pass to the client application as per the [client authentication protocol](#client-authentication-protocol).

**Authorization**: This request must be sent to the canister with `caller` that is the self-authenticating id derived from any of the public keys of devices associated with the user before this call.

## The Internet Identity Service backend internals

This section, which is to be expanded, describes interesting design choices about the internals of the Internet Identity Service Canister. In particular

### Salt

The `salt` used to blind the hashes that form the `seed` of the Canister Signature "public keys" is obtained via a call to `aaaaa-aa.raw_rand()`. The resulting 32 byte sequence is used as-is.

Since this cannot be done during `canister_init` (no calls from canister init), the randomness is fetched by someone triggering the `init_salt()` method explicitly, or just any other update call. More concretely:

-   Anyone can invoke `init_salt()`

-   `init_salt()` traps if `salt != EMPTY_SALT`

-   Else, `init_salt()` calls `aaaaa-aa.raw_rand()`. When that comes back successfully, and *still* `salt == EMPTY_SALT`, it sets the salt. Else, it traps (so that even if it is run multiple times concurrently, only the first to write the salt has an effect).

-   *all* other update methods, at the beginning, if `salt == EMPTY_SALT`, they await `self.init_salt()`, ignoring the result (even if it is an error). Then they check if we still have `salt == EMPTY_SALT` and trap if that is the case.

### Why we do not use `canister_inspect_message`

The system allows canisters to inspect ingress messages before they are actually ingressed, and decide if they want to pay for them (see [the interface spec](https://internetcomputer.org/docs/current/references/ic-interface-spec/#system-api-inspect-message)). Because the Internet Identity canisters run on a system subnet, cycles are not actually charged, but we still want to avoid wasting resources.

It seems that this implies that we should use `canister_inspect_message` to reject messages that would, for example, not pass authentication.

But upon closer inspection (heh), this is not actually useful.

-   One justification for this mechanism would be if we expect a high number of accidentally invalid calls. But we have no reason to expect them at the moment.

-   Another is to protect against a malicious actor. But that is only useful if the malicious actor doesn't have an equally effective attack vector anyways, and in our case they do: If they want to flood the NNS with calls, they can use calls that do authenticate (e.g. keeping removing and adding devices, or preparing delegations); these calls would pass message inspection.

On the flip side, implementing `canister_inspect_message` adds code, and thus a risk for bugs. In particular it increases the risk that some engineer might wrongly assume that the authentication check in `canister_inspect_message` is sufficient and will not do it again in the actual method, which could lead to a serious bug.

Therefore the Internet Identity Canister intentionally does not implement `canister_inspect_message`.

### Internal data model and data structures used

The primary data structure used by the backend is a map from Identity Anchor to the list of user devices. Device lists are stored directly in canister stable memory. The total amount of storage is limited to 2KiB bytes per user. With the stable memory size of 8GiB we can store around `4 * 10^6` user records in a single canister.

#### Stable memory layout

All the integers (u64, u32, u16) are encoded in Little-Endian.

    Storage ::= {
      Header
      UserRecords
    }

    Header ::= {
      magic : u8[3] = "IIC"
      version : u8 = 1
      number_of_user_records : u32
      user_number_range_lo : u64
      user_number_range_hi : u64
      entry_size: u16
      salt: u8[32]
      padding : u8[454]
    }

    UserRecords ::= UserRecord*

    UserRecord ::= {
      size : u16
      candid_bytes: u8[510]
    }

User record for Identity Anchor N is stored at offset `sizeof(Header) + (N - user_number_range_lo) * sizeof(UserRecord)`. Each record consists of a 16 bit `size` ∈ \[0..510\] followed by `size` bytes of Candid-serialized list of devices.

    type UserDeviceList = vec(record {
      pubkey : DeviceKey;
      alias : text;
      credential_id : opt CredentialId;
    });

### Initialization

The Internet Identity canister is designed for sharded deployments. There can be many simultaneously installed instances of the canister code, each serving requests of a subset of users. As users are identified by their Identity Anchor, we split the range of Identity Anchors into continuous non-overlapping half-closed intervals and assign each region to one canister instance. The assigned range is passed to the canister as an init argument, encoded in Candid:

    type InternetIdentityInit = record {
      // Half-closed interval of Identity Anchors assigned to this canister, [ left_bound, right_bound )
      assigned_user_number_range: record { nat64; nat64; };
    };

### Approach to upgrades

We don't need any logic recovery logic in pre/post-upgrade hooks because we place all user data to stable memory in a way that can be accessed directly. The signature map is simply dropped on upgrade, so users will have to re-request their delegations.

## The Internet Identity Service frontend

The Internet Identity Service frontend is the user-visible part of the Internet Identity Service, and where it all comes together. It communicates with

-   the user

-   its backend using the Candid interface described above

-   the security devices, using the Web Authentication API

-   its past and future self, via the browser storage

-   client application frontends, via the OAUTH protocol

### Storage used

The frontend only stores a single piece of local storage, namely the current Identity Anchor, if known under the key `user_number`.

### Flows

The following flows are not prescriptive of the UI, e.g. "the frontend asks the user for X" may also mean that on the previous shown page, there is already a field for X.

The possible login subflows are shared among entry points `/` and `/authorized`, and are thus described separately. At the end of a successful login subflow:

-   The frontend knows the `user_number` (also stored in local storage).

-   the frontend has a temporary session key

-   the frontend has a `device_identity` for the present security device

-   the frontend has a `frontend_delegation` from the security device to the session key

All update calls to the Internet Identity Service Backend are made under the `device_identity` and are signed with the session key.

:::info
The steps marked with 👆 are the steps where the user presses the security device.
:::

### Subflow: Login as returning user

1.  The frontend notices that `user_number` is present in local storage.

2.  The frontend offers the choices

    -   Welcome `<Identity Anchor>`. Do you want to log in?

    -   Log in as a different user

3.  User wants to log in

4.  The frontend uses `lookup` to fetch the list of devices

5.  The frontend creates a session key.

6.  👆 The frontend creates a delegation from the security device key to the session key, and signs it with the security key, using any of the devices listed in the user account. It notes which device was actually used.

    Let `device_identity` of type `WebAuthenicationIdentity` be the identity created from that, and let `frontend_delegation` be the signed delegation.

7.  The frontend configures the agent to use the session key for all further update calls.

8.  Login complete

### Subflow: Login via initial registration

1.  The frontend notices that no `user_number` is present in local storage.

2.  The frontend offers the choices

    -   Create new account

    -   Log into existing account with existing device

    -   Log into existing account with new device

3.  The user chooses to create a new account

4.  👆 The frontend asks the security device to create a new public key. Let `device_identity` of type `WebAuthenicationIdentity` be the identity created from that.

5.  The frontend creates a session key.

6.  👆 The frontend creates a delegation from the security device key to the session key, and signs it with the security key. Let `frontend_delegation` be that signed delegation.

7.  The frontend configures the agent to use the session key for all further update calls.

8.  The frontend asks the user for a device alias.

9.  The frontend calls `register()`, and obtains the `user_number`.

10. It stores the `user_number` in local storage.

11. The frontend insistently tells the user to write down this number.

12. The frontend asks the user to create a recovery option (see Flow: Setup Recovery)

13. Login complete

### Subflow: Login via existing device

1.  The frontend notices that no `user_number` is present in local storage. (Or user said "log in as different user" in returning flow.)

2.  The frontend offers the choices

    -   Create new account

    -   Log into existing account with existing device

    -   Log into existing account with new device

3.  The user selects "Log into existing account with existing device"

4.  The frontend asks the user for their Identity Anchor, and stores that in `user_number`.

5.  Continue as in "Subflow: Login as returning user"

### Subflow: Login via new device

1.  The frontend notices that no `user_number` is present in local storage.

2.  The frontend offers the choices

    -   Create new account

    -   Log into existing account with existing device

    -   Log into existing account with new device

3.  The user selects "Log into existing account with new device"

4.  The frontend asks the user for their Identity Anchor.

5.  The frontend asks the user for a device alias.

6.  👆 Frontend asks security device for a new public key and credential id.

7.  The frontend calls `add_tentative_device()`

8.  The frontend polls `lookup()` until the credential id is present or the timeout is reached.

9.  The frontend stores the `user_number` in local storage

10. Login complete

(See ["Flow: adding remote device"](#flow-adding-remote-device) for what happens on the other device.)

### Flow: Direct access to the frontend

This flow is the boring default

1.  User browses to `https://identity.ic0.app/`

2.  👆 The appropriate login subflow happens

3.  User sees their management screen. In particular

    -   Their Identity Anchor

    -   A button to add additional devices

    -   The list of their devices, with device aliases, a symbol marking recovery devices, and a button to remove

    -   A "logout" button

:::note
One could imagine additional information, such as the last time a device was used, or even a list of recent client applications that the user logged into.
:::

### Flow: adding remote device

1.  The user accesses `https://identity.ic0.app/`

2.  👆 The appropriate login subflow happens

3.  On the management page the user selects \"Add New Device\"

4.  On the flow selection screen the user selects \"Remote Device\"

    -   Call `enter_device_registration_mode()` to start device registration flow

5.  The UI polls `get_anchor_info()` until a tentative device is present or the timeout is reached

    -   Now the "Flow: Login via new device" happens on the remote device

6.  The user enters the verification code

    -   Call `verify_tentative_device()` to complete the flow

### Flow: Setup recovery

1.  The user is offered two options for recovery and the option to skip

    -   A security key

    -   A BIP-39 seed phrase

2.  Depending on their choice a) If they choose the seed phrase it is generated and displayed to them with a copy button and an explanation of having to keep this phrase secure b) 👆 If they choose the security key they generate new credentials for the touched device

3.  The device is added to the users account with the purpose `#recovery` set

### Flow: Use recovery

1.  On the login page the user selects "Recover my account"

2.  The user is prompted for their Identity Anchor

3.  If no `DeviceData` with the purpose `#recovery` is found for the Identity Anchor an error is displayed

4.  The user is asked to provide the seed phrase or 👆 their recovery security key

5.  The management page is shown

### Flow: authenticating client applications

1.  The user accesses `/#authorize`

2.  👆 The appropriate login subflow happens

3.  The frontend listens to a `message` event (as per [`postMessage` API](https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage))

4.  The `event.data` should be a message as per our [Client authentication protocol](#client-authentication-protocol).

5.  If the `message` contains a value for `derivationOrigin`:
    * If the `derivationOrigin` value does not match the format specified in the section [Client authentication protocol](#client-authentication-protocol), the process flow is aborted with a failure message.
    * The frontend calls `https://<canister_id>.ic0.app/.well-known/ii-alternative-origins` to retrieve the allowed alter native origins.
    * If there is no such _certified_ asset the flow is aborted with a failure message.
    * The frontend validates the retrieved data as per schema specified in section [JSON Schema](#alternative-frontend-origins-schema)
    * If the `event.origin` is contained in the array value of the property `alternativeOrigins` the `derivationOrigin` will be used as the Application Frontend’s hostname.

6. If the `message` does _not_ contain a value for `derivationOrigin` the `event.origin` is used as the Application Frontend’s hostname

7. The user is asked if they want to log into the client application, showing the client application frontend’s hostname.

8.  The frontend calls `prepare_delegation()` with the client application frontend hostname, client application provided session key and desired time to live.

9.  The frontend queries `get_delegation()` to get the delegation data

10. It posts that data to the client application, using `event.source.postMessage` and the types specified in [Client authentication protocol](#client-authentication-protocol).

11. After receiving the data the client application is expected to close the Internet Identity window / tab.

### Flow: Deleting devices

1.  The user is logged in, on the management view, and selects a device to delete.

2.  If this is the device the user is currently logged in (the current `device_identity`), the user is warned.

3.  If this is the last device of the user, the user is warned even more sternly.

4.  The device is removed via `remove()`.

5.  If this was the device that the user has logged in with, log out (as per "Flow: logging out")

6.  Else, refresh the device view.

### Flow: Logging out

1.  The user is logged in, on the management view, and clicks the logout button.

2.  The `user_number` is removed from local storage

3.  The page is reloaded (to send the user back to the beginning of "Flow: Direct access").

