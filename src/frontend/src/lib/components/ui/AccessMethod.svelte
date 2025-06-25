<script lang="ts">
  import {
    type OpenIdCredential,
    type DeviceWithUsage,
  } from "$lib/generated/internet_identity_types";
  import { formatLastUsage } from "$lib/utils/time";
  import { nonNullish } from "@dfinity/utils";
  import { fade } from "svelte/transition";
  import PlaceHolder from "./PlaceHolder.svelte";

  let {
    accessMethod,
  }: { accessMethod: DeviceWithUsage | OpenIdCredential | null } = $props();

  const formatOpenIdCredentialName = (credential: OpenIdCredential) => {
    const metadataName = credential.metadata.find(
      ([key, _val]) => key === "name",
    )?.[1]!;
    if (metadataName && "String" in metadataName) {
      return metadataName.String;
    }
    const metadataEmail = credential.metadata.find(
      ([key, _val]) => key === "email",
    )?.[1]!;
    if (metadataEmail && "String" in metadataEmail) {
      return metadataEmail.String;
    }
  };
</script>

{#if accessMethod}
  {#if "credential_id" in accessMethod}
    <h5
      class="text-text-primary text-sm font-semibold nth-[2]:hidden"
      transition:fade={{ delay: 30 }}
    >
      <div class="mr-3 inline-block">
        {accessMethod.alias}
      </div>
      {#if nonNullish(accessMethod.last_usage[0])}
        <div class="text-text-tertiary inline-block font-normal">
          Last used {formatLastUsage(
            new Date(Number(accessMethod.last_usage[0] / BigInt(1000000))),
          )}
        </div>
      {/if}
    </h5>
  {:else}
    <h5
      class="text-text-primary text-sm font-semibold nth-[2]:hidden"
      transition:fade={{ delay: 30 }}
    >
      <div class="mr-3 inline-block">
        {formatOpenIdCredentialName(accessMethod)}
      </div>

      {#if nonNullish(accessMethod.last_usage_timestamp[0])}
        <div class="text-text-tertiary ml-3 font-normal">
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
