<script lang="ts">
  import {
    type OpenIdCredential,
    type AuthnMethodData,
  } from "$lib/generated/internet_identity_types";
  import { formatLastUsage } from "$lib/utils/time";
  import { nonNullish } from "@dfinity/utils";
  import { fade } from "svelte/transition";
  import PlaceHolder from "./PlaceHolder.svelte";

  let {
    accessMethod,
  }: { accessMethod: AuthnMethodData | OpenIdCredential | null } = $props();

  const getAuthnMethodAlias = (authnMethod: AuthnMethodData) => {
    const metadataAlias = authnMethod.metadata.find(
      ([key, _val]) => key === "alias",
    )?.[1]!;
    if (metadataAlias && "String" in metadataAlias) {
      return metadataAlias.String;
    }
  };

  const getOpenIdCredentialName = (credential: OpenIdCredential) => {
    const metadataEmail = credential.metadata.find(
      ([key, _val]) => key === "email",
    )?.[1]!;
    if (metadataEmail && "String" in metadataEmail) {
      return metadataEmail.String;
    }
    const metadataName = credential.metadata.find(
      ([key, _val]) => key === "name",
    )?.[1]!;
    if (metadataName && "String" in metadataName) {
      return metadataName.String;
    }
  };
</script>

{#if accessMethod}
  {#if "authn_method" in accessMethod}
    <!-- Passkey -->
    <h5
      class="text-text-primary text-sm font-semibold nth-[2]:hidden"
      transition:fade={{ delay: 30, duration: 30 }}
    >
      <div class="mr-3 inline-block min-w-32">
        {getAuthnMethodAlias(accessMethod)}
      </div>
      {#if nonNullish(accessMethod.last_authentication[0])}
        <div class="text-text-tertiary inline-block font-normal">
          Last used {formatLastUsage(
            new Date(
              Number(accessMethod.last_authentication[0] / BigInt(1000000)),
            ),
          )}
        </div>
      {/if}
    </h5>
  {:else}
    <!-- OpenID -->
    <h5
      class="text-text-primary text-sm font-semibold nth-[2]:hidden"
      transition:fade={{ delay: 30, duration: 30 }}
    >
      <div class="mr-3 inline-block min-w-32">
        {getOpenIdCredentialName(accessMethod)}
      </div>

      {#if nonNullish(accessMethod.last_usage_timestamp[0])}
        <div class="text-text-tertiary inline-block font-normal">
          Last used {formatLastUsage(
            new Date(
              Number(accessMethod.last_usage_timestamp[0] / BigInt(1000000)),
            ),
          )}
        </div>
      {/if}
    </h5>
  {/if}
{:else}
  <PlaceHolder class="mr-8 h-4 !rounded-sm" />
{/if}
