import type { Channel, JsonRequest } from "$lib/utils/transport/utils";

const supportedStandards = [
  {
    name: "ICRC-25",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_25_signer_interaction_standard.md",
  },
  {
    name: "ICRC-29",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_29_window_post_message_transport.md",
  },
  {
    name: "ICRC-34",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_34_delegation.md",
  },
  {
    name: "ICRC-95",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_95_derivationorigin.md",
  },
];

const scopes = [{ method: "icrc34_delegation" }];

/** ICRC-25: respond with the list of supported standards. */
export const handleSupportedStandards =
  (channel: Channel) => (request: JsonRequest) => {
    if (
      request.id === undefined ||
      request.method !== "icrc25_supported_standards"
    ) {
      return;
    }
    void channel.send({
      jsonrpc: "2.0",
      id: request.id,
      result: { supportedStandards },
    });
  };

/** ICRC-25: respond with granted permission scopes. */
export const handlePermissions =
  (channel: Channel) => (request: JsonRequest) => {
    if (
      request.id === undefined ||
      (request.method !== "icrc25_permissions" &&
        request.method !== "icrc25_request_permissions")
    ) {
      return;
    }
    void channel.send({
      jsonrpc: "2.0",
      id: request.id,
      result: {
        scopes: scopes.map((scope) => ({ scope, state: "granted" })),
      },
    });
  };
