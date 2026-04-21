/**
 * Channel store — owns the full lifecycle of the post-message channel between
 * Internet Identity and the relying party (dapp).
 *
 * Responsibilities:
 *  - Establishing the channel via PostMessage or Legacy transport.
 *  - Responding to ICRC-25 standard requests (supported standards, permissions).
 *  - Handling ICRC-34 delegation requests: waits for the user to authenticate
 *    and authorize, validates the derivation origin, creates the delegation
 *    chain, and sends the response.
 *  - Exposing error and idle state for the UI to react to.
 */
import { derived, get, type Readable, writable } from "svelte/store";
import type { ChannelOptions, Transport } from "$lib/utils/transport/utils";
import { PostMessageTransport } from "$lib/utils/transport/postMessage";
import { PostMessageUnsupportedError } from "$lib/utils/transport/postMessage";
import { LegacyTransport } from "$lib/utils/transport/legacy";
import { frontendCanisterConfig, getPrimaryOrigin } from "$lib/globals";
import type { Channel } from "$lib/utils/transport/utils";
import {
  handleSupportedStandards,
  handlePermissions,
} from "$lib/stores/channelHandlers/icrc25";
import { handleDelegationRequest } from "$lib/stores/channelHandlers/delegation";
import {
  handleLegacyAttributes,
  handleIcrc3Attributes,
} from "$lib/stores/channelHandlers/attributes";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export type ChannelError =
  | "unable-to-connect"
  | "connection-closed"
  | "invalid-request"
  | "unverified-origin"
  | "unsupported-browser"
  | "delegation-failed"
  | "attributes-failed";

type ChannelStore = Readable<Channel | undefined> & {
  establish: (options?: ChannelOptions) => void;
};

// ---------------------------------------------------------------------------
// Transports
// ---------------------------------------------------------------------------

const getTransports = (): Transport[] => {
  const primaryOrigin = getPrimaryOrigin();
  return [
    new PostMessageTransport(),
    new LegacyTransport(
      // Redirect requests and responses between related origins and primary origin
      primaryOrigin !== undefined
        ? {
            redirectToOrigin: primaryOrigin,
            trustedOrigins: frontendCanisterConfig.related_origins[0] ?? [],
          }
        : undefined,
    ),
  ];
};

// ---------------------------------------------------------------------------
// Stores
// ---------------------------------------------------------------------------

/** Set when the channel encounters an unrecoverable error. */
export const channelErrorStore = writable<ChannelError | undefined>();

const channelInternalStore = writable<Channel | undefined>();

export const channelStore: ChannelStore = {
  subscribe: channelInternalStore.subscribe,
  establish: (options) => {
    (async () => {
      const existingChannel = get(channelInternalStore);
      if (existingChannel?.closed === false) {
        return;
      }

      const channel = await Promise.any(
        getTransports().map((transport) => transport.establishChannel(options)),
      );
      channelInternalStore.set(channel);

      const onError = (error: ChannelError) => channelErrorStore.set(error);

      channel.addEventListener("request", handleSupportedStandards(channel));
      channel.addEventListener("request", handlePermissions(channel));
      channel.addEventListener(
        "request",
        handleDelegationRequest(channel, onError),
      );
      channel.addEventListener(
        "request",
        handleLegacyAttributes(channel, onError),
      );
      channel.addEventListener(
        "request",
        handleIcrc3Attributes(channel, onError),
      );
      channel.addEventListener("close", () =>
        channelErrorStore.set("connection-closed"),
      );
    })().catch((error) => {
      console.error(error);
      // Promise.any wraps errors in AggregateError
      const errors = error instanceof AggregateError ? error.errors : [error];
      if (
        errors.some((e: unknown) => e instanceof PostMessageUnsupportedError)
      ) {
        channelErrorStore.set("unsupported-browser");
        return;
      }
      channelErrorStore.set("unable-to-connect");
    });
  },
};

/** Convenience derived store that throws when the channel is not yet established. */
export const establishedChannelStore: Readable<Channel> = derived(
  channelStore,
  (channel) => {
    if (channel === undefined) {
      throw new Error("Channel has not been established yet");
    }
    return channel;
  },
);
