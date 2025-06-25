<script lang="ts">
  import Panel from "$lib/components/ui/Panel.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { ChevronRight, InfoIcon } from "@lucide/svelte";
  import ListItem from "$lib/components/ui/ListItem.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { type DeviceWithUsage } from "$lib/generated/internet_identity_types";
  import PlaceHolder from "$lib/components/ui/PlaceHolder.svelte";
  import { fade } from "svelte/transition";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import { formatLastUsage } from "$lib/utils/time";

  const getLastUsedAccessMethod = (
    devices: DeviceWithUsage[],
  ): DeviceWithUsage | null => {
    if (devices.length === 0) {
      return null;
    }
    return devices.sort((devA, devB) => {
      if (nonNullish(devA.last_usage[0]) && nonNullish(devB.last_usage[0])) {
        return Number(devA.last_usage[0]) - Number(devB.last_usage[0]);
      } else if (
        isNullish(devA.last_usage[0]) &&
        nonNullish(devB.last_usage[0])
      ) {
        return 1;
      } else if (
        nonNullish(devA.last_usage[0]) &&
        isNullish(devB.last_usage[0])
      ) {
        return -1;
      } else {
        return 0;
      }
    })[0];
  };

  const lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(identityInfo.devices),
  );
</script>

<div>
  <div class="mh-9 mb-4">
    <h1 class="text-text-primary text-3xl font-semibold">
      Welcome,
      {#if !identityInfo.name}
        <PlaceHolder class="mt-0.5 inline-block h-6 w-64" />
      {:else}
        <span transition:fade={{ delay: 30 }}>
          {identityInfo.name}!
        </span>
      {/if}
    </h1>
  </div>
  <h2 class="text-text-tertiary mb-12">
    Manage your identity and passkeys below.
  </h2>

  <Panel>
    <div class="p-4">
      <h3 class="text-text-primary mb-2 text-lg font-semibold">My Identity</h3>
      <h4 class="text-text-tertiary text-sm">
        Internet Identity is used to sign in securely and connect to apps with
        passkeys.
      </h4>
    </div>
    <ul>
      <ListItem>
        <h5 class="text-text-tertiary min-w-30 text-sm">Identity Name</h5>
        <div class="flex-1">
          {#if identityInfo.name}
            <h5
              class="text-text-primary text-sm font-semibold nth-[2]:hidden"
              transition:fade={{ delay: 30 }}
            >
              {identityInfo.name}
            </h5>
          {:else}
            <PlaceHolder class="mr-8 h-4 !rounded-sm" />
          {/if}
        </div>
        <Tooltip
          label="Your Identity name is currently not editable. It is only ever visible to you."
          ><InfoIcon class="text-text-primary" /></Tooltip
        >
      </ListItem>
      <ListItem href="manage/security">
        <h5 class="text-text-tertiary min-w-30 text-sm nth-[2]:hidden">
          Access Methods
        </h5>
        <div class="flex-1">
          {#if lastUsedAccessMethod}
            <h5
              class="text-text-primary text-sm font-semibold nth-[2]:hidden"
              transition:fade={{ delay: 30 }}
            >
              <div class="mr-3 inline-block">
                {lastUsedAccessMethod.alias}
              </div>
              {#if nonNullish(lastUsedAccessMethod.last_usage[0])}
                <div class="text-text-tertiary inline-block font-normal">
                  Last used {formatLastUsage(
                    new Date(
                      Number(
                        lastUsedAccessMethod.last_usage[0] / BigInt(1000000),
                      ),
                    ),
                  )}
                </div>
              {/if}
            </h5>
          {:else}
            <PlaceHolder class="mr-8 h-4 !rounded-sm" />
          {/if}
        </div>
        <ChevronRight class="text-text-primary" />
      </ListItem>
    </ul>
  </Panel>
</div>
