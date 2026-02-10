import { derived, get, type Readable, writable } from "svelte/store";
import type {
  Channel,
  ChannelOptions,
  JsonRequest,
  JsonResponse,
  Transport,
} from "$lib/utils/transport/utils";
import { PostMessageTransport } from "$lib/utils/transport/postMessage";
import { LegacyTransport } from "$lib/utils/transport/legacy";
import type {
  PermissionScope,
  SupportedStandard,
} from "@slide-computer/signer";
import { canisterConfig, getPrimaryOrigin } from "$lib/globals";

type ChannelStore = Readable<Channel | undefined> & {
  establish: (options?: ChannelOptions) => Promise<Channel>;
};

const primaryOrigin = getPrimaryOrigin();
const transports: Transport[] = [
  new PostMessageTransport(),
  new LegacyTransport(
    // Redirect requests and responses between related origins and primary origin
    primaryOrigin !== undefined
      ? {
          redirectToOrigin: primaryOrigin,
          trustedOrigins: canisterConfig.related_origins[0] ?? [],
        }
      : undefined,
  ),
];

const supportedStandards: SupportedStandard[] = [
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
const scopes: PermissionScope[] = [
  {
    method: "icrc34_delegation",
  },
];

const supportedStandardsListener =
  (channel: Channel) => (request: JsonRequest) => {
    if (
      request.id === undefined ||
      request.method !== "icrc25_supported_standards"
    ) {
      return;
    }
    const response: JsonResponse = {
      id: request.id,
      jsonrpc: "2.0",
      result: { supportedStandards },
    };
    void channel.send(response);
  };
const permissionsListener = (channel: Channel) => (request: JsonRequest) => {
  if (
    request.id === undefined ||
    (request.method !== "icrc25_permissions" &&
      request.method !== "icrc25_request_permissions")
  ) {
    return;
  }
  const response: JsonResponse = {
    id: request.id,
    jsonrpc: "2.0",
    result: {
      scopes: scopes.map((scope) => ({
        scope,
        state: "granted",
      })),
    },
  };
  void channel.send(response);
};

const internalStore = writable<Channel | undefined>();

export const channelStore: ChannelStore = {
  subscribe: internalStore.subscribe,
  establish: async (options): Promise<Channel> => {
    // Return existing channel if already established and not closed
    const existingChannel = get(internalStore);
    if (existingChannel?.closed === false) {
      return existingChannel;
    }
    // Else establish channel
    const channel = await Promise.any(
      transports.map((transport) => transport.establishChannel(options)),
    );
    // Return default responses for ICRC-25 requests
    channel.addEventListener("request", supportedStandardsListener(channel));
    channel.addEventListener("request", permissionsListener(channel));
    // Return channel after setting internal store
    internalStore.set(channel);
    return channel;
  },
};

export const establishedChannelStore: Readable<Channel> = derived(
  channelStore,
  (channel) => {
    if (channel === undefined) {
      throw new Error("Channel has not been established yet");
    }
    return channel;
  },
);
