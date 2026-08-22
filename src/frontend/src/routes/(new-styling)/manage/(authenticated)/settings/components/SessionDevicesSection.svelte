<script lang="ts">
  import { MonitorSmartphoneIcon } from "@lucide/svelte";
  import { Trans } from "$lib/components/locale";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { toaster } from "$lib/components/utils/toaster";
  import { signOutSessionDevice, type SessionDevice } from "../sessionDevices";

  interface Props {
    identityNumber: bigint;
    devices: SessionDevice[];
  }

  const { identityNumber, devices }: Props = $props();
  const titleId = $props.id();

  let signedOut = $state<number[]>([]);
  let signingOut = $state<number | undefined>(undefined);

  const handleSignOut = async (device: SessionDevice) => {
    signingOut = device.id;
    try {
      await signOutSessionDevice(
        $authenticatedStore.actor,
        identityNumber,
        device.id,
        device.isCurrent,
      );
      signedOut = [...signedOut, device.id];
    } catch (error) {
      toaster.error({
        title: $t`Couldn't sign this browser out`,
        description: error instanceof Error ? error.message : undefined,
      });
    } finally {
      signingOut = undefined;
    }
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-row items-start gap-3 rounded-xl border p-4 sm:gap-4 sm:p-5"
>
  <span
    class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
    aria-hidden="true"
  >
    <MonitorSmartphoneIcon class="size-5" />
  </span>

  <div class="flex min-w-0 flex-1 flex-col gap-3">
    <div class="flex flex-col gap-1">
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`Signed-in browsers`}
      </h3>
      <p class="text-text-tertiary text-sm">
        {#if devices.length === 0}
          <Trans>
            Apps you sign in to from a browser will show up here, so you can end
            their access at any time.
          </Trans>
        {:else}
          <Trans>
            Signing a browser out ends its access to every app it is signed in
            to.
          </Trans>
        {/if}
      </p>
    </div>

    {#if devices.length > 0}
      <ul class="flex flex-col gap-2" aria-labelledby={titleId}>
        {#each devices as device (device.id)}
          {@const lastUsed = new Date(device.lastUsedMillis)}
          <li
            class="border-border-tertiary bg-bg-primary flex flex-row items-center gap-3 rounded-lg border p-3"
          >
            <div class="flex min-w-0 flex-1 flex-col">
              <span class="flex min-w-0 flex-row items-center gap-2">
                <span class="text-text-primary truncate text-sm">
                  {device.name}
                </span>
                {#if device.isCurrent}
                  <Tooltip
                    label={$t`Signing out this browser ends the sessions you are using right now.`}
                    direction="up"
                  >
                    <Badge
                      color="success"
                      size="sm"
                      dot
                      class="flex-none cursor-default select-none"
                      tabindex={0}>{$t`This browser`}</Badge
                    >
                  </Tooltip>
                {/if}
              </span>
              <span class="flex flex-row items-center gap-1.5">
                <Tooltip
                  label={$formatDate(lastUsed, {
                    timeStyle: "short",
                    dateStyle: "medium",
                  })}
                  direction="up"
                  align="start"
                >
                  <span class="text-text-tertiary cursor-default text-xs">
                    {$t`Last used ${$formatRelative(lastUsed, { style: "long" })}`}
                  </span>
                </Tooltip>
                <!-- Names repeat across browsers, so the entry needs something that does not. -->
                <span class="text-text-tertiary text-xs">#{device.id}</span>
              </span>
            </div>
            {#if signedOut.includes(device.id)}
              <span class="text-text-tertiary shrink-0 text-sm">
                {$t`Signed out`}
              </span>
            {:else}
              <button
                class="btn btn-secondary btn-sm shrink-0"
                disabled={signingOut !== undefined}
                onclick={() => handleSignOut(device)}
              >
                {signingOut === device.id ? $t`Signing out…` : $t`Sign out`}
              </button>
            {/if}
          </li>
        {/each}
      </ul>
    {/if}
  </div>
</section>
