import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
import { isNotifiableOrigin } from "$lib/utils/notifications/notifiableOrigin";
import { get } from "svelte/store";

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
    name: "ICRC-167",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_167_browser_url_transport.md",
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

const scopes = (origin: string) => [
  { method: "icrc34_delegation" },
  { method: "ii_session_delegation" },
  // Only while the feature is on, and only for an origin consent can be keyed by:
  // an app that saw the scope would otherwise call a method that cannot succeed.
  ...(get(PUSH_NOTIFICATIONS) && isNotifiableOrigin(origin)
    ? [{ method: "ii_notification_consent" }]
    : []),
];

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
        scopes: scopes(channel.origin).map((scope) => ({
          scope,
          state: "granted",
        })),
      },
    });
  };
