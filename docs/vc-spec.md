# II Verifiable Credential Spec

## Window Post Message Interface

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
  * `issuers`: An array of issuers that the relying party trusts. Each issuer is an object with the following properties:
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
    "issuers": [
      {
        "issuerOrigin": "https://nns.ic0.app",
        "credentialId": "credential1"
      },
      {
        "issuerOrigin": "https://example.com",
        "credentialId": "credential2"
      }
    ],
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

