<script lang="ts">
  import { channelStore } from "$lib/stores/channelStore";
  import { t } from "$lib/stores/locale.store";
  import {
    type Channel,
    type ChannelOptions,
    DelegationParamsCodec,
    INVALID_PARAMS_ERROR_CODE,
  } from "$lib/utils/transport/utils";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import {
    resetIdleTimeout,
    startIdleTimeout,
  } from "$lib/stores/authorize-flow.store";
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

  const authorizeChannel = (channel: Channel): Promise<void> =>
    new Promise<void>((resolve, reject) => {
      let resolved = false;

      // When any recognized request arrives, reset the idle timeout
      // so the flow stays active.
      channel.addEventListener("request", (request) => {
        if (request.id === undefined) {
          return;
        }
        if (
          request.method === "icrc34_delegation" ||
          request.method === "ii-icrc3-attributes"
        ) {
          resetIdleTimeout();
        }
      });

      // Handle delegation requests
      channel.addEventListener("request", async (request) => {
        if (
          request.id === undefined ||
          request.method !== "icrc34_delegation" ||
          $authorizationStore !== undefined
        ) {
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
          if (!resolved) {
            resolved = true;
            resolve();
          }
        } catch (error) {
          console.error(error);
          reject(
            new AuthorizeChannelError(
              $t`Unverified origin`,
              $t`It seems like the request could not be processed.`,
            ),
          );
        }
      });

      // Handle standalone attribute requests (resolve auth UI if not already)
      channel.addEventListener("request", (request) => {
        if (
          request.id === undefined ||
          request.method !== "ii-icrc3-attributes"
        ) {
          return;
        }
        authorizationStore.setOrigin(channel.origin);
        if (!resolved) {
          resolved = true;
          resolve();
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
        // Start the idle timeout after each response is sent.
        channel.addEventListener("response", () => {
          startIdleTimeout();
        });
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
