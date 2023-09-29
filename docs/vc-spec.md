# II Verifiable Credential Spec (MVP)


## Issuer API

This section describes the (Candid) interface to be implemented by an issuer of verifiable credentials on the IC.
This interface is used by the II-canister during attribute sharing flow (cf. [flow description](https://github.com/dfinity/wg-identity-authentication/blob/main/topics/attribute-sharing.md))
An example implementation of the interface is given in [demos/vc_issuer](../demos/vc_issuer).

The Candid interface is as follows, and the subsequent sections describe the
services and the corresponding messages in more detail.

```candid
type CredentialSpec = record { info : text };
type GetCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    vc_jwt : opt text;
    credential_spec : CredentialSpec;
};
type GetCredentialResponse = variant {
    Ok : IssuedCredentialData;
    Err : IssueCredentialError
};
type Icrc21ConsentInfo = record { consent_message : text; language : text };
type Icrc21ConsentMessageRequest = record {
    arg : vec nat8;
    method : text;
    preferences : Icrc21ConsentPreferences;
};
type Icrc21ConsentMessageResponse = variant {
    Ok : Icrc21ConsentInfo;
    Err : Icrc21Error;
};
type Icrc21ConsentPreferences = record { language : text };
type Icrc21Error = variant {
    GenericError : Icrc21ErrorInfo;
    MalformedCall : Icrc21ErrorInfo;
    NotSupported : Icrc21ErrorInfo;
    Forbidden : Icrc21ErrorInfo;
};
type Icrc21ErrorInfo = record { description : text; error_code : nat64 };
type IssueCredentialError = variant {
    Internal : text;
    SignatureNotFound;
    InvalidIdAlias;
    UnauthorizedSubject;
    UnknownSubject;
};
type IssuedCredentialData = record { vc_jws : text };
type PrepareCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    credential_spec : CredentialSpec;
};
type PrepareCredentialResponse = variant {
    Ok : PreparedCredentialData;
    Err : IssueCredentialError;
};
type PreparedCredentialData = record { vc_jwt : opt text };
type SignedIdAlias = record {
    credential_jws : text;
    id_alias : principal;
    id_dapp : principal;
};
service : {
    consent_message : (Icrc21ConsentMessageRequest) -> (
    Icrc21ConsentMessageResponse,
    );
    get_credential : (GetCredentialRequest) -> (GetCredentialResponse) query;
    prepare_credential : (PrepareCredentialRequest) -> (
    PrepareCredentialResponse,
    );
}
```

### 1: Consent Message

In the attribute sharing flow a user must approve the issuance of a verifiable
credential by an issuer, and this happens by approving a human-readable consent message from the issuer.
See [ICRC-21](https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_21_consent_msg.md) for more detail.

Identity provider requests the consent message via `Icrc21ConsentMessageRequest`,
and upon successful response displays the consent message from `Icrc21ConsentInfo` to the user.

### 2: Prepare Credential

Preparation of a credential involves checking the validity of the request,
and upon success, preparation of the actual credential requested by the user.

```candid
service : {
  prepare_credential : (PrepareCredentialRequest) -> (PrepareCredentialResponse, );
};

type PrepareCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    credential_spec : CredentialSpec;
};

type SignedIdAlias = record {
    credential_jws : text;
    id_alias : principal;
    id_dapp : principal;
};

type PrepareCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    credential_spec : CredentialSpec;
};

type PrepareCredentialResponse = variant {
    Ok : PreparedCredentialData;
    Err : IssueCredentialError;
};

type PreparedCredentialData = record { vc_jwt : opt text };
```

Specifically, the issuer checks via `prepared_id_alias.credential_jws` that user identified via `id_dapp` on the issuer
side can be identified using `id_alias` for the purpose of attribute sharing, and that the credential
described by `credential_spec` does apply to the user.  When these checks are successful, the issuer
prepares and returns the credential in JWT format.

**NOTE**: `prepare_credential`-service is needed only for some issuers, e.g. those that use [canister signatures](https://internetcomputer.org/docs/current/references/ic-interface-spec/#canister-signatures)
during the credential issuance.  For issuers that don't need "preparation"-step, this operation
can be essentially a no-op that immediately returns `PrepareCredentialResponse::Ok` with an empty
`PreparedCredentialData.vc_jwt`.

### 3: Get Credential

`get_credential`-service issues the actual credential requested by the user.

```candid
service : {
  get_credential : (GetCredentialRequest) -> (GetCredentialResponse) query;
};

type GetCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    vc_jwt : opt text;
    credential_spec : CredentialSpec;
};
type GetCredentialResponse = variant {
    Ok : IssuedCredentialData;
    Err : IssueCredentialError;
};
type IssuedCredentialData = record { vc_jws : text };

```

`GetCredentialRequest` should contain the same parameters as `PrepareCredentialRequest`, plus the
prepared JWT-credential returned by `prepare_credential`, if any.
The issuer performs the same checks as during the `prepare_credential`-call,
plus verify that the JWT-credential is consistent with the other parameters.

Upon successful checks, issuer returns the signed credential in JWS-format.




##  Relying Party API (Window Post Message Interface)

### 1: Load II in a new window

The II window needs to be opened with the subpath `/vc-flow`.

After opening the II window, II will load and notify `window.opener` with the following JSON-RPC notification:

```json
{
  "jsonrpc": "2.0",
  "method": "vc-flow-ready"
}
```

### 2: Request a VC

After receiving the notification that II is ready, the relying party can request a VC by sending the following JSON-RPC request:
 
* Method: `request_credential`
* Params:
  * `issuer`: An issuer that the relying party trusts. It has the following properties:
    * `issuerOrigin`: The origin of the issuer.
    * `credentialId`: The ID of the credential that the relying party wants to request from the issuer.
  * `credentialSubject`: The subject of the credential as known to the relying party. Internet Identity will use this principal to ensure that the flow is completed using the matching identity.

#### Example

```json
{
  "id": 1,
  "jsonrpc": "2.0",
  "method": "request_credential",
  "params": {
    "issuer": {
        "issuerOrigin": "https://example-issuer.com",
        "credentialId": "age_credential"
    },
    "credentialSubject": "2mdal-aedsb-hlpnv-qu3zl-ae6on-72bt5-fwha5-xzs74-5dkaz-dfywi-aqe"
  }
}
```

### 3: Receive Verifiable Presentation

After the user has successfully completed the flow, Internet Identity will respond with the following JSON-RPC response:

* Result:
  * `verifiablePresentation`: The JWT based verifiable presentation containing two credentials: 
    1. A verifiable credential issued by Internet Identity that relates the requested `credentialSubject` to another alias principal.
    2. A verifiable credential issued by on of the requested issuers issued to the alias principal.

#### Example

```json
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": {
    "verifiablePresentation": "eyJQ..."
  }
}
```

An example of such a verifiable presentation can be found [here](https://www.w3.org/TR/vc-data-model/#example-jwt-payload-of-a-jwt-based-verifiable-presentation-non-normative).

#### Interaction Model

Given the interactive nature of the flow, the relying party should not expect to receive a response immediately. Instead, the relying party should wait for one of the following events:
* II sends a JSON-RPC response as described above.
* II sends a JSON-RPC error response.
* The user closes the II window. This should be treated as an error.

The relying party may also close the II window after some timeout. The user should then be notified by the relying party that the flow failed.

