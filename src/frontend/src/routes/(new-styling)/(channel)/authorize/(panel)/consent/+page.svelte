<script lang="ts">
  import { onMount } from "svelte";
  import { authorizationContextStore } from "$lib/stores/authorization.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { establishedChannelStore } from "$lib/stores/channelStore";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { t } from "$lib/stores/locale.store";
  import { handleError } from "$lib/components/utils/error";
  import {
    Icrc3AttributesParamsSchema,
    type JsonRequest,
  } from "$lib/utils/transport/utils";
  import { throwCanisterError, retryFor } from "$lib/utils/utils";
  import { z } from "zod";
  import Button from "$lib/components/ui/Button.svelte";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { ShieldCheckIcon } from "@lucide/svelte";
  import {
    buildConsentGroups,
    isImplicitConsentAttribute,
    needsConsentScreen,
    type ConsentGroup,
  } from "./utils";

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );

  let groups = $state<ConsentGroup[]>([]);
  let checkedKeys = $state(new Set<string>());
  let pendingRequest = $state<{
    request: JsonRequest & { id: string | number };
    nonce: string;
    issuer?: string;
  } | null>(null);

  onMount(() => {
    // Listen for attribute requests on the channel.
    // Since no response was sent yet, the pending request will be emitted.
    const unsubscribe = $establishedChannelStore.addEventListener(
      "request",
      async (request: JsonRequest) => {
        if (
          request.id === undefined ||
          request.method !== "ii-icrc3-attributes"
        ) {
          return;
        }
        const result = Icrc3AttributesParamsSchema.safeParse(request.params);
        if (result.success) {
          await handleAttributeRequest(
            { ...request, id: request.id },
            result.data.keys,
            result.data.nonce,
          );
        }
      },
    );

    return unsubscribe;
  });

  const handleAttributeRequest = async (
    request: JsonRequest & { id: string | number },
    requestedKeys: string[],
    nonce: string,
  ) => {
    try {
      // TODO: get issuer from context if available (OpenID flow)
      const issuer = undefined;

      const availableAttributes = await $authenticatedStore.actor
        .list_available_attributes({
          identity_number: $authenticatedStore.identityNumber,
          attributes: [requestedKeys],
        })
        .then(throwCanisterError);

      const consentGroups = buildConsentGroups(
        requestedKeys,
        availableAttributes,
        [], // TODO: pass metadata for provider names/logos
        issuer,
      );

      groups = consentGroups;
      pendingRequest = { request, nonce, issuer };

      // Pre-check implicit-consent attributes
      for (const group of consentGroups) {
        for (const option of group.options) {
          if (option.implicit) {
            checkedKeys.add(option.scopedKey);
          }
        }
      }
    } catch (error) {
      handleError(error);
    }
  };

  const handleContinue = async () => {
    if (pendingRequest === null) {
      return;
    }

    const selectedKeys = Array.from(checkedKeys);
    const { request, nonce } = pendingRequest;

    try {
      await sendIcrc3AttributeResponse(request, selectedKeys, nonce);
    } catch (error) {
      await $establishedChannelStore.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: 1000,
          message:
            "Encountered an internal error while processing the request.",
        },
      });
    }
  };

  const sendIcrc3AttributeResponse = async (
    request: JsonRequest & { id: string | number },
    selectedKeys: string[],
    nonce: string,
  ) => {
    const { message } = await $authenticatedStore.actor
      .prepare_icrc3_attributes({
        origin: $authorizationContextStore.effectiveOrigin,
        account_number: [],
        identity_number: $authenticatedStore.identityNumber,
        attributes: selectedKeys.map((key) => ({
          key,
          value: [],
          omit_scope: false,
        })),
        nonce: z.util.base64ToUint8Array(nonce),
      })
      .then(throwCanisterError);
    const { signature } = await retryFor(5, () =>
      $authenticatedStore.actor
        .get_icrc3_attributes({
          origin: $authorizationContextStore.effectiveOrigin,
          account_number: [],
          identity_number: $authenticatedStore.identityNumber,
          message,
        })
        .then(throwCanisterError),
    );
    await $establishedChannelStore.send({
      jsonrpc: "2.0",
      id: request.id,
      result: {
        data: z.util.uint8ArrayToBase64(new Uint8Array(message)),
        signature: z.util.uint8ArrayToBase64(new Uint8Array(signature)),
      },
    });
  };

  const handleDenyAll = async () => {
    // Clear all non-implicit selections
    checkedKeys = new Set(
      groups
        .flatMap((g) => g.options)
        .filter((o) => o.implicit)
        .map((o) => o.scopedKey),
    );
    await handleContinue();
  };

  const toggleAttribute = (scopedKey: string) => {
    if (checkedKeys.has(scopedKey)) {
      checkedKeys.delete(scopedKey);
    } else {
      checkedKeys.add(scopedKey);
    }
    checkedKeys = new Set(checkedKeys); // trigger reactivity
  };
</script>

<div class="flex flex-col items-center gap-6">
  <div class="flex flex-col items-center gap-2 text-center">
    <FeaturedIcon>
      <ShieldCheckIcon />
    </FeaturedIcon>
    <h2 class="text-text-primary text-xl font-semibold">
      {$t`Review Permissions`}
    </h2>
    <p class="text-text-secondary text-sm">
      {$t`Choose which details you'd like to share`}
    </p>
  </div>

  {#if groups.length > 0}
    <div class="flex w-full flex-col gap-3">
      {#each groups as group}
        {#if group.options.length === 1}
          {@const option = group.options[0]}
          <Checkbox
            checked={checkedKeys.has(option.scopedKey)}
            onchange={() => toggleAttribute(option.scopedKey)}
            label={group.label}
            hint={option.value}
          />
        {:else}
          <!-- Multiple options: show label + select -->
          <div class="flex flex-col gap-1">
            <p class="text-text-secondary text-sm font-medium">
              {group.label}
            </p>
            <!-- TODO: Replace with proper dropdown for multi-provider selection -->
            {#each group.options as option}
              <Checkbox
                checked={checkedKeys.has(option.scopedKey)}
                onchange={() => toggleAttribute(option.scopedKey)}
                label={option.providerName ?? option.scopedKey}
                hint={option.value}
              />
            {/each}
          </div>
        {/if}
      {/each}
    </div>
  {/if}

  <div class="flex w-full flex-col gap-2">
    <Button size="lg" onclick={handleContinue}>
      {$t`Continue`}
    </Button>
    <Button variant="tertiary" size="lg" onclick={handleDenyAll}>
      {$t`Deny All`}
    </Button>
  </div>
</div>
