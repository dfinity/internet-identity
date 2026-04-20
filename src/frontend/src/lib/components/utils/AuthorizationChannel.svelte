<script lang="ts">
  import { channelStore } from "$lib/stores/channelStore";
  import { t } from "$lib/stores/locale.store";
  import {
    type Channel,
    type ChannelOptions,
    DelegationParamsCodec,
    Icrc3AttributesParamsSchema,
    INVALID_PARAMS_ERROR_CODE,
  } from "$lib/utils/transport/utils";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { retryFor, throwCanisterError } from "$lib/utils/utils";
  import { z } from "zod";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import { type Snippet } from "svelte";
  import { goto } from "$app/navigation";
  import { PostMessageUnsupportedError } from "$lib/utils/transport/postMessage";

  class AuthorizeChannelError extends Error {
    #title: string;
    #description: string;

    constructor(title: string, description: string) {
      super(`${title}: ${description}`);
      this.#title = title;
      this.#description = description;
    }

    get title() {
      return this.#title;
    }

    get description() {
      return this.#description;
    }
  }

  type Props = {
    options?: ChannelOptions;
    children: Snippet;
  };

  const { options, children }: Props = $props();

  /** Handle ICRC-3 attribute requests in the default (non-OpenID) flow.
   *  Responds with only the implicit entries (nonce, origin, timestamp)
   *  since the user did not authenticate via OpenID. */
  const handleIcrc3AttributeRequests = (channel: Channel) => {
    channel.addEventListener("request", async (request) => {
      if (
        request.id === undefined ||
        request.method !== "ii-icrc3-attributes"
      ) {
        return;
      }
      const paramsResult = Icrc3AttributesParamsSchema.safeParse(
        request.params,
      );
      if (!paramsResult.success) {
        await channel.send({
          jsonrpc: "2.0",
          id: request.id,
          error: {
            code: INVALID_PARAMS_ERROR_CODE,
            message: z.prettifyError(paramsResult.error),
          },
        });
        return;
      }
      try {
        // Wait for the user to authorize and authenticate first.
        await new Promise<void>((resolve) => {
          const unsub = authorizationStore.subscribe((value) => {
            if (value !== undefined) {
              queueMicrotask(() => unsub());
              resolve();
            }
          });
        });
        const { identityNumber, actor } = $authenticatedStore;
        const origin = $authorizationStore!.effectiveOrigin;
        const { message } = await actor
          .prepare_icrc3_attributes({
            origin,
            account_number: [],
            identity_number: identityNumber,
            attributes: [],
            nonce: z.util.base64ToUint8Array(paramsResult.data.nonce),
          })
          .then(throwCanisterError);
        const { signature } = await retryFor(5, () =>
          actor
            .get_icrc3_attributes({
              origin,
              account_number: [],
              identity_number: identityNumber,
              message,
            })
            .then(throwCanisterError),
        );
        await channel.send({
          jsonrpc: "2.0",
          id: request.id,
          result: {
            data: z.util.uint8ArrayToBase64(new Uint8Array(message)),
            signature: z.util.uint8ArrayToBase64(new Uint8Array(signature)),
          },
        });
      } catch (error) {
        console.error(error);
      }
    });
  };

  const authorizeChannel = (channel: Channel): Promise<void> =>
    new Promise<void>((resolve, reject) => {
      // Only handle ICRC-3 attribute requests in the default (non-OpenID) flow.
      // The OpenID resume flow has its own handler.
      if (
        options === undefined ||
        (!options.pending && !options.allowedOrigin)
      ) {
        handleIcrc3AttributeRequests(channel);
      }

      channel.addEventListener("request", async (request) => {
        if (
          request.id === undefined ||
          request.method !== "icrc34_delegation" ||
          $authorizationStore !== undefined
        ) {
          // Ignore if it's a different method, or we're already processing
          return;
        }
        const result = DelegationParamsCodec.safeParse(request.params);
        if (!result.success) {
          await channel.send({
            jsonrpc: "2.0",
            id: request.id,
            error: {
              code: INVALID_PARAMS_ERROR_CODE,
              message: z.prettifyError(result.error),
            },
          });
          reject(
            new AuthorizeChannelError(
              $t`Invalid request`,
              $t`It seems like an invalid authentication request was received.`,
            ),
          );
          return;
        }
        try {
          await authorizationStore.handleRequest(
            channel.origin,
            request.id,
            result.data,
          );
          resolve();
        } catch (error) {
          console.error(error); // Log error to console
          reject(
            new AuthorizeChannelError(
              $t`Unverified origin`,
              $t`It seems like the request could not be processed.`,
            ),
          );
        }
      });
    });

  let authorizePromise = $state(
    channelStore
      .establish(options)
      .catch((error) => {
        console.error(error); // Log error to console
        if (error instanceof PostMessageUnsupportedError) {
          goto("/unsupported");
          return new Promise<Channel>(() => {}); // Never resolve since we render the unsupported page
        }
        return Promise.reject(
          new AuthorizeChannelError(
            $t`Unable to connect`,
            $t`There was an issue connecting with the application. Try a different browser; if the issue persists, contact the developer.`,
          ),
        );
      })
      .then((channel) => {
        if (options?.pending === true) {
          // Don't authorize if we're only doing an initial handshake
          return;
        }
        // Replace promise when channel closes after it was established
        channel.addEventListener("close", () => {
          authorizePromise = Promise.reject(
            new AuthorizeChannelError(
              $t`Connection closed`,
              $t`It seems like the connection with the service could not be established. Try a different browser; if the issue persists, contact support.`,
            ),
          );
        });
        return authorizeChannel(channel);
      }),
  );
</script>

{#await authorizePromise then _}
  {@render children()}
{:catch error}
  {@const title =
    error instanceof AuthorizeChannelError ? error.title : $t`Unexpected error`}
  {@const message =
    error instanceof AuthorizeChannelError
      ? error.description
      : error instanceof Error
        ? error.message
        : $t({
            message: "Something went wrong",
            context:
              "Fallback error message when an unexpected error is caught",
          })}
  <Dialog>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">{title}</h1>
    <p class="text-text-tertiary mb-6 text-base font-medium">{message}</p>
    <Button onclick={() => window.close()} variant="secondary">
      <RotateCcwIcon class="size-4" />
      <span>{$t`Return to app`}</span>
    </Button>
  </Dialog>
{/await}
