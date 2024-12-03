# II Verifiable Credential Spec (MVP)

## Issuer API

This section describes the (Candid) interface to be implemented by an issuer of verifiable credentials on the IC.
This interface is used by the II-canister during attribute sharing flow (cf. [flow description](https://github.com/dfinity/wg-identity-authentication/blob/d2664795afe9cea40386804bdb1259a47e34540d/topics/attribute-sharing.md))
An example implementation of the interface is given in [demos/vc_issuer](https://github.com/dfinity/internet-identity/tree/main/demos/vc_issuer).

The Candid interface is as follows, and the subsequent sections describe the
services and the corresponding messages in more detail.

```candid
// Specification of a requested credential.
type CredentialSpec = record {
    credential_type : text;
    /// arguments are optional, and specific to the credential_type
    arguments : opt vec record { text; ArgumentValue };
};
type ArgumentValue = variant { "Int" : int32; String : text };

/// Types for ICRC-21 consent message, cf.
/// https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_21_consent_msg.md
type Icrc21ConsentInfo = record { consent_message : text; language : text };
type Icrc21ConsentPreferences = record { language : text };
type Icrc21Error = variant {
    GenericError : record { description : text; error_code : nat };
    UnsupportedCanisterCall : Icrc21ErrorInfo;
    ConsentMessageUnavailable : Icrc21ErrorInfo;
};
type Icrc21ErrorInfo = record { description : text };
type Icrc21VcConsentMessageRequest = record {
    preferences : Icrc21ConsentPreferences;
    credential_spec : CredentialSpec;
};

/// Types for `prepare_credential`.
type PrepareCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    credential_spec : CredentialSpec;
};
type SignedIdAlias = record {
    credential_jws : text;
};
type PreparedCredentialData = record { prepared_context : opt vec nat8 };

/// Types for `get_credential`.
type GetCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    credential_spec : CredentialSpec;
    prepared_context : opt blob;
};
type IssuedCredentialData = record { vc_jws : text };

type IssueCredentialError = variant {
    /// The caller is not known to the issuer.  Caller should register first with the issuer before retrying.
    UnknownSubject : text;
    /// The caller is not authorized to obtain the requested credential.  Caller requested a credential
    /// for a different principal, or the issuer does not have sufficient knowledge about the caller
    /// to issue the requested credential.
    UnauthorizedSubject : text;
    /// The id_alias credential provided by the identity provider is invalid.
    InvalidIdAlias : text;
    /// The issuer does not issue credentials described in the credential spec.
    UnsupportedCredentialSpec : text;
    /// Internal errors, indicate malfunctioning of the issuer.
    SignatureNotFound : text;
    Internal : text;
};

/// Types for `derivation_origin`.
type DerivationOriginRequest = record {
    frontend_hostname : text;
};
type DerivationOriginData = record { origin : text };
type DerivationOriginError = variant {
  Internal : text;
  UnsupportedOrigin : text;
};

service: {
    derivation_origin : (DerivationOriginRequest) ->
        (variant {Ok: DerivationOriginData; Err: DerivationOriginError});
    vc_consent_message : (Icrc21VcConsentMessageRequest) ->
        (variant { Ok : Icrc21ConsentInfo; Err : Icrc21Error;});
    prepare_credential : (PrepareCredentialRequest) ->
        (variant { Ok : PreparedCredentialData; Err : IssueCredentialError;});
    get_credential : (GetCredentialRequest) ->
        (variant { Ok : IssuedCredentialData; Err : IssueCredentialError;}) query;
}
```

### 1: Consent Message

In the attribute sharing flow a user must approve the issuance of a verifiable
credential by an issuer, and this happens by approving a human-readable consent message from the issuer.

Identity provider uses a VC-extension of [ICRC-21](https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_21_consent_msg.md), and requests the consent message via `Icrc21VcConsentMessageRequest`,
Upon successful response identity provider displays the consent message from `Icrc21ConsentInfo` to the user.

### 2: Derivation Origin

The issuer must implement also `derivation_origin`-API, which allows for taking the advantage
of the [Alternative Derivation Origins](https://internetcomputer.org/docs/current/references/ii-spec#alternative-frontend-origins)-feature.
`derivation_origin` is called by the identity provider to obtain an URL to be used as the derivation origin
for user's principal. If an issuer doesn't use the _Alternative Derivation Origins_-feature,
the function should return just the default value, namely the canister's URL: `https://<issuer-canister-id>.icp0.io`.

```candid
service: {
    derivation_origin : (DerivationOriginRequest) ->
        (variant {Ok: DerivationOriginData; Err: DerivationOriginError});
}

type DerivationOriginRequest = record {
    frontend_hostname : text;
};
type DerivationOriginData = record { origin : text };
type DerivationOriginError = variant {
  Internal : text;
  UnsupportedOrigin : text;
};
```

Please note that the returned derivation origin is subject to verification via
`.well-known/ii-alternative-origins`, as described in the [feature-description](https://internetcomputer.org/docs/current/references/ii-spec#alternative-frontend-origins).

### 3: Prepare Credential

Preparation of a credential involves checking the validity of the request,
and upon success, preparation of the actual credential requested by the user.

```candid
service : {
    prepare_credential : (PrepareCredentialRequest) ->
        (variant { Ok : PreparedCredentialData; Err : IssueCredentialError;});
};

type PrepareCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    credential_spec : CredentialSpec;
};

type SignedIdAlias = record {
    credential_jws : text;
};
type PreparedCredentialData = record { prepared_context : opt vec nat8 };

type CredentialSpec = record {
    credential_type : text;
    /// arguments are optional, and specific to the credential_type
    arguments : opt vec record { text; ArgumentValue };
};
type ArgumentValue = variant { "Int" : int32; String : text };
```

Specifically, the issuer checks via `signed_id_alias.credential_jws` that user identified by its `sub` claim on the
issuer side has a valid `has_id_alias` principal for the purpose of attribute sharing, and that the credential
described by `credential_spec` does apply to the `caller`. When these checks are successful, the issuer
prepares and returns a context in `PreparedCredentialData.prepared_context` (if any). The returned prepared context is then
passed back to the issuer in a subsequent `get_credential`-call (see below).
This call must be authenticated, i.e. the sender must match the principal for which the credential is requested.

**NOTE:**
The value of `prepared_context` is basically used to transfer information between `prepare_credential`
and `get_credential` steps, and it is totally up to the issuer to decide on the content of
that field. That is, the issuer creates `prepared_context`, and is the only entity that
consumes it. For example, when using [canister signatures](https://internetcomputer.org/docs/current/references/ic-interface-spec#canister-signatures)
the context contains a time-stamped yet unsigned VC, for which the canister signature will be
available only at `get_credential`-call.

**NOTE:**
The convention is to sign the credential with the domain separator `"iccs_verifiable_credential"` as an array of bytes. This is achieved by adding a prefix that consists of a 1-byte lenth of the domain separator, followed by the actual separator. See [vc_signing_input_hash](https://docs.rs/ic-verifiable-credentials/1.0.1/ic_verifiable_credentials/fn.vc_signing_input_hash.html).

### 4: Get Credential

`get_credential`-service issues the actual credential requested by the user.

```candid
service : {
    get_credential : (GetCredentialRequest) ->
        (variant { Ok : IssuedCredentialData; Err : IssueCredentialError;}) query;
};

type GetCredentialRequest = record {
    signed_id_alias : SignedIdAlias;
    credential_spec : CredentialSpec;
    prepared_context : opt blob;
};
type IssuedCredentialData = record { vc_jws : text };
```

`GetCredentialRequest` should contain the same parameters as `PrepareCredentialRequest`, plus the
`prepared_context`-value returned by `prepare_credential`, if any.
The issuer performs the same checks as during the `prepare_credential`-call,
plus verify that `prepared_context` is consistent with the other parameters.
This call must be authenticated, i.e. the sender must match the principal for which the credential is requested.

Upon successful checks, issuer returns the signed credential in JWS-format.

### Recommended convention connecting credential specification with the returned credentials.

Service discovery and syntax of the claims in the returned credentials are out of scope
of this spec (of the MVP service). However, an issuer may follow the convention below,
for an easier verification of the returned credentials.

Given a credential spec like

```json
    "credentialSpec": {
        "credentialType": "SomeVerifiedProperty",
        "arguments": {
            "argument_1": "value_1",
            "another_argument": 42,
        }
```

the returned JWT should contain in `credentialSubject` a property
named by the value of `credentialType` from the spec, with key-value
entries listing the arguments from the spec, namely

```json
    "SomeVerifiedProperty": {
        "argument_1": "value_1",
        "another_argument": 42,
    }
```

For example, for `VerifiedAdult`-credential we'd use the following credential spec

```json
    "credentialSpec": {
        "credentialType": "VerifiedAdult",
        "arguments": {
            "minAge": 18,
    }
```

and a compliant issuer would issue a VC that contains `credentialSubject` with the property

```json
    "VerifiedAdult": {
        "minAge": 18,
    }
```

## Identity Provider API

This section describes the [_`window.postMessage()`_](https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage)-interface implemented by the identity provider (Internet Identity).
This interface is used by a Relying Party during attribute sharing flow (cf. [flow description](https://github.com/dfinity/wg-identity-authentication/blob/main/topics/attribute-sharing.md))

### 1: Load II in a new window

The II window needs to be opened with the URL path `/vc-flow`.

After opening the II window, II will load and notify `window.opener` with the following JSON-RPC notification:

```json
{
  "jsonrpc": "2.0",
  "method": "vc-flow-ready"
}
```

### 2: Request a VC

After receiving the notification that II is ready, the relying party can request a VC by sending the following JSON-RPC request:

- Method: `request_credential`
- Params:
  - `issuer`: An issuer that the relying party trusts. It has the following properties:
    - `origin`: The front-end origin of the issuer. If this value is different from the value returned from the `derivation_origin` canister call, then the `origin` must be a valid alternative origin as per the [Alternative Frontend Origins](https://internetcomputer.org/docs/current/references/ii-spec#alternative-frontend-origins)-feature.
    - `canisterId`: The canister id of the issuer canister (i.e. the one, that implements the candid issuer API as defined above).
  - `credentialSpec`: The spec of the credential that the relying party wants to request from the issuer.
    - `credentialType`: The type of the requested credential.
    - `arguments`: (optional) A map with arguments specific to the requested credentials. It maps string keys to values that must be either strings or integers.
  - `credentialSubject`: The subject of the credential as known to the relying party. Internet Identity will use this principal to ensure that the flow is completed using the matching identity.
  - `derivationOrigin`: (optional) The origin that should be used for principal derivation (instead of the client origin) during the verification of `credentialSubject` (applicable if the relying party
    uses the [Alternative Frontend Origins](https://internetcomputer.org/docs/current/references/ii-spec#alternative-frontend-origins)-feature).

#### Examples

```json
{
  "id": 1,
  "jsonrpc": "2.0",
  "method": "request_credential",
  "params": {
    "issuer": {
      "origin": "https://employment-info.com",
      "canisterId": "rwlgt-iiaaa-aaaaa-aaaaa-cai"
    },
    "credentialSpec": {
      "credentialType": "VerifiedEmployee",
      "arguments": {
        "employerName": "XYZ Ltd."
      }
    },
    "credentialSubject": "2mdal-aedsb-hlpnv-qu3zl-ae6on-72bt5-fwha5-xzs74-5dkaz-dfywi-aqe"
  }
}
```

```json
{
  "id": 1,
  "jsonrpc": "2.0",
  "method": "request_credential",
  "params": {
    "issuer": {
      "origin": "https://kyc-star.com",
      "canisterId": "rdmx6-jaaaa-aaaaa-aaadq-cai"
    },
    "credentialSpec": {
      "credentialType": "VerifiedAdult",
      "arguments": {
        "minAge": 21
      }
    },
    "credentialSubject": "s33qc-ctnp5-ubyz4-kubqo-p2tem-he4ls-6j23j-hwwba-37zbl-t2lv3-pae",
    "derivationOrigin": "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io"
  }
}
```

```json
{
  "id": 1,
  "jsonrpc": "2.0",
  "method": "request_credential",
  "params": {
    "issuer": {
      "origin": "https://kyc-resident-info.org",
      "canisterId": "rwlgt-iiaaa-aaaaa-aaaaa-cai"
    },
    "credentialSpec": {
      "credentialType": "VerifiedResident",
      "arguments": {
        "countryName": "Panama",
        "countryAlpha2": "PA"
      }
    },
    "credentialSubject": "cpehq-54hef-odjjt-bockl-3ldtg-jqle4-ysi5r-6bfah-v6lsa-xprdv-pqe"
  }
}
```

### 3: Get a Response

#### Receive a Verifiable Presentation

After the user has successfully completed the flow, Internet Identity will respond with the following JSON-RPC response:

- `verifiablePresentation`: The JWT based verifiable presentation containing two credentials:
  1. A verifiable credential issued by Internet Identity that relates the requested `credentialSubject` to another alias principal.
  2. A verifiable credential issued by the requested issuer to the alias principal.

**NOTE:** the order of the credentials in the presentation should match the list above,
i.e. the credential that relates the subject to alias principal should come first.

##### Example

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

#### Receive an Error Message

If the flow failed for any reason, Internet Identity returns an error.
For privacy protection, the error does not give any details on the root cause of the failure.
(This may change in the future, as some failures can be reported to the relying
party without impacting user's privacy.)

##### Example

```json
{
  "id": 1,
  "jsonrpc": "2.0",
  "error": {
    "version": "1",
    "code": "UNKNOWN"
  }
}
```

#### Interaction Model

Given the interactive nature of the flow, the relying party should not expect to receive a response immediately. Instead, the relying party should wait for one of the following events:

- II sends a JSON-RPC response as described above.
- II sends a JSON-RPC error response.
- The user closes the II window. This should be treated as an error.

The relying party may also close the II window after some timeout. The user should then be notified by the relying party that the flow failed.
