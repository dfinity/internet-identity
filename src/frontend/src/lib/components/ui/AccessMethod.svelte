<script lang="ts">
  import {
    type OpenIdCredential,
    type AuthnMethodData,
  } from "$lib/generated/internet_identity_types";
  import { formatLastUsage } from "$lib/utils/time";
  import { nonNullish } from "@dfinity/utils";
  import { fade } from "svelte/transition";
  import PlaceHolder from "./PlaceHolder.svelte";
  import Ellipsis from "../utils/Ellipsis.svelte";
  import PulsatingCircleIcon from "../icons/PulsatingCircleIcon.svelte";
  import { getAuthnMethodAlias } from "$lib/utils/webAuthn";

  let {
    accessMethod,
    class: classes,
    isCurrent,
  }: {
    accessMethod: AuthnMethodData | OpenIdCredential | null;
    class?: string;
    isCurrent?: boolean;
  } = $props();

  const getOpenIdCredentialName = (credential: OpenIdCredential | null) => {
    if (!credential) return null;
    const metadataName = credential.metadata.find(
      ([key, _val]) => key === "name",
    )?.[1]!;
    if (metadataName && "String" in metadataName) {
      return metadataName.String;
    }
    return undefined;
  };

  const getOpenIdCredentialEmail = (credential: OpenIdCredential | null) => {
    if (!credential) return null;
    const metadataEmail = credential.metadata.find(
      ([key, _val]) => key === "email",
    )?.[1]!;
    if (metadataEmail && "String" in metadataEmail) {
      return metadataEmail.String;
    }
    return undefined;
  };

  let openIdHasName = $derived(
    accessMethod &&
      !("authn_method" in accessMethod) &&
      !!getOpenIdCredentialName(accessMethod),
  );
  let openIdHasEmail = $derived(
    accessMethod &&
      !("authn_method" in accessMethod) &&
      !!getOpenIdCredentialEmail(accessMethod),
  );
</script>

{#if accessMethod}
  {#if "authn_method" in accessMethod}
    <!-- Passkey -->
    <div
      class={[
        "text-text-primary foldable-subgrid text-sm font-semibold nth-[2]:hidden",
        classes,
      ]}
      in:fade={{ delay: 30, duration: 30 }}
      out:fade={{ duration: 30 }}
    >
      <div class="flex min-w-32 items-center pr-3">
        {getAuthnMethodAlias(accessMethod)}
        {#if isCurrent}
          <span class="ml-2" aria-label="Current Passkey">
            <PulsatingCircleIcon />
          </span>
        {/if}
      </div>
      {#if isCurrent}
        <div
          class="text-text-tertiary flex items-center font-normal md:justify-end"
        >
          <span>Last used now</span>
        </div>
      {:else if nonNullish(accessMethod.last_authentication[0])}
        <div
          class="text-text-tertiary flex items-center font-normal md:justify-end"
        >
          Last used {formatLastUsage(
            new Date(
              Number(accessMethod.last_authentication[0] / BigInt(1000000)),
            ),
          )}
        </div>
      {/if}
    </div>
  {:else}
    <!-- OpenID -->
    <div
      class={[
        "text-text-primary foldable-subgrid text-sm font-semibold nth-[2]:hidden",
        classes,
      ]}
      in:fade={{ delay: 30, duration: 30 }}
      out:fade={{ duration: 30 }}
    >
      {#if openIdHasName && openIdHasEmail}
        <div class="flex min-w-32 flex-col justify-center pr-3">
          <div class="flex items-center gap-2">
            <span>
              {getOpenIdCredentialName(accessMethod)}
            </span>
            {#if isCurrent}
              <PulsatingCircleIcon />
            {/if}
          </div>
          <div class="text-text-tertiary font-extralight">
            <Ellipsis text={getOpenIdCredentialEmail(accessMethod)!}></Ellipsis>
          </div>
        </div>
      {:else if !openIdHasName && openIdHasEmail}
        <div class="flex min-w-32 items-center gap-2 pr-3">
          <Ellipsis text={getOpenIdCredentialEmail(accessMethod)!}></Ellipsis>
          {#if isCurrent}
            <PulsatingCircleIcon />
          {/if}
        </div>
      {:else if openIdHasName && !openIdHasEmail}
        <div class="flex min-w-32 items-center gap-2 pr-3">
          <span>
            {getOpenIdCredentialName(accessMethod)}
          </span>
          {#if isCurrent}
            <PulsatingCircleIcon />
          {/if}
        </div>
      {/if}

      {#if isCurrent}
        <div
          class="text-text-tertiary flex items-center font-normal md:justify-end"
        >
          Last used now
        </div>
      {:else if nonNullish(accessMethod.last_usage_timestamp[0])}
        <div
          class="text-text-tertiary flex items-center font-normal md:justify-end"
        >
          Last used {formatLastUsage(
            new Date(
              Number(accessMethod.last_usage_timestamp[0] / BigInt(1000000)),
            ),
          )}
        </div>
      {:else}
        <div></div>
      {/if}
    </div>
  {/if}
{:else}
  <!-- <div
    class="flex items-center"
    in:fade={{ duration: 30 }}
    out:fade={{ duration: 30, delay: 30 }}
  > -->
  <PlaceHolder
    class="mt-1 mr-8 hidden h-4 !rounded-sm nth-last-[2]:inline-block"
    hiddenIndex={7}
  />
  <!-- </div> -->
{/if}

<style>
  .foldable-subgrid {
    display: grid;
    /* We may need to update the minimum width for really long emails, but it seems fine for now.  */
    grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
  }
</style>
