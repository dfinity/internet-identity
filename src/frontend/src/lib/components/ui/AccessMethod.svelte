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

  let {
    accessMethod,
    class: classes,
  }: {
    accessMethod: AuthnMethodData | OpenIdCredential | null;
    class?: string;
  } = $props();

  const getAuthnMethodAlias = (authnMethod: AuthnMethodData) => {
    const metadataAlias = authnMethod.metadata.find(
      ([key, _val]) => key === "alias",
    )?.[1]!;
    if (metadataAlias && "String" in metadataAlias) {
      return metadataAlias.String;
    }
  };

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
      transition:fade={{ delay: 30, duration: 30 }}
    >
      <div class="flex min-w-32 items-center pr-3">
        {getAuthnMethodAlias(accessMethod)}
      </div>
      {#if nonNullish(accessMethod.last_authentication[0])}
        <div class="text-text-tertiary flex items-center font-normal">
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
      transition:fade={{ delay: 30, duration: 30 }}
    >
      {#if openIdHasName && openIdHasEmail}
        <div class="flex min-w-32 flex-col justify-center pr-3">
          <div>{getOpenIdCredentialName(accessMethod)}</div>
          <div class="text-text-tertiary font-extralight">
            <Ellipsis text={getOpenIdCredentialEmail(accessMethod)!}></Ellipsis>
          </div>
        </div>
      {:else if !openIdHasName && openIdHasEmail}
        <div class="flex min-w-32 items-center pr-3">
          <Ellipsis text={getOpenIdCredentialEmail(accessMethod)!}></Ellipsis>
        </div>
      {:else if openIdHasName && !openIdHasEmail}
        <div class="min-w-32 pr-3">
          {getOpenIdCredentialName(accessMethod)}
        </div>
      {/if}

      {#if nonNullish(accessMethod.last_usage_timestamp[0])}
        <div class="text-text-tertiary flex items-center font-normal">
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
  <PlaceHolder class="mr-8 h-4 !rounded-sm" />
{/if}

<style>
  .foldable-subgrid {
    display: grid;
    /* We may need to update the minimum width for really long emails, but it seems fine for now.  */
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  }
</style>
