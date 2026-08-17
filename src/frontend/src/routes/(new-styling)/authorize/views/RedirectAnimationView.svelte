<script lang="ts">
  import { establishedChannelStore } from "$lib/stores/channelStore";
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import { t } from "$lib/stores/locale.store";
  import { draw, fade, scale } from "svelte/transition";
  import { cubicOut } from "svelte/easing";
  import Logo from "$lib/components/ui/Logo.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import { waitFor } from "$lib/utils/utils";
  import BreatheSparkleCanvas from "$lib/components/backgrounds/BreatheSparkleCanvas.svelte";

  const metadataStore = $derived(
    getAppMetadataStore($establishedChannelStore.origin),
  );
  const dapp = $derived($metadataStore);
  const hostname = $derived(new URL($establishedChannelStore.origin).hostname);
</script>

<div class="flex min-h-[100dvh] flex-col items-center justify-center px-8">
  <div
    in:scale={{ duration: 500, easing: cubicOut, start: 0.9 }}
    class="flex flex-col items-center justify-center"
  >
    {#if dapp.logo !== undefined}
      <div class="relative">
        <svg viewBox="0 0 92 92" width="92" height="92" class="mb-4">
          <path
            d="M 46 1 H 71 A 20 20 0 0 1 91 21 V 71 A 20 20 0 0 1 71 91 H 21 A 20 20 0 0 1 1 71 V 21 A 20 20 0 0 1 21 1 H 46"
            class="stroke-fg-primary/10 fill-none stroke-2"
          />
          <g
            in:fade|global={{
              duration: 500,
              easing: cubicOut,
              delay: 500,
            }}
          >
            <path
              in:draw|global={{
                duration: 6000,
                easing: cubicOut,
                delay: 500,
              }}
              d="M 46 1 H 71 A 20 20 0 0 1 91 21 V 71 A 20 20 0 0 1 71 91 H 21 A 20 20 0 0 1 1 71 V 21 A 20 20 0 0 1 21 1 H 46"
              stroke-linecap="round"
              class="stroke-fg-primary breathe fill-none stroke-2"
            />
          </g>
        </svg>
        <img
          src={dapp.logo}
          alt={$t`${dapp.name ?? hostname} logo`}
          class="absolute inset-1 size-[84px] rounded-[18px] object-cover"
        />
      </div>
    {:else}
      <svg viewBox="0 0 92 92" width="92" height="92" class="mb-4">
        <circle
          cx="46"
          cy="46"
          r="45"
          stroke-width="2"
          class="stroke-fg-primary/10 fill-none stroke-2"
        />
        <g
          in:fade|global={{
            duration: 500,
            easing: cubicOut,
            delay: 500,
          }}
        >
          <circle
            in:draw|global={{
              duration: 6000,
              easing: cubicOut,
              delay: 500,
            }}
            cx="46"
            cy="46"
            r="45"
            stroke-linecap="round"
            class="stroke-fg-primary breathe origin-center -rotate-90 fill-none stroke-2"
          />
        </g>
      </svg>
    {/if}
    <!-- The logo above is app-provided (permissionless) metadata, so the
         app's origin stays visible next to it as the trust anchor the user
         can verify — same pairing as on the authorize screens. -->
    <Badge size="sm" class="mb-4 max-w-75">
      <Ellipsis text={hostname} position="middle" />
    </Badge>
    <p class="text-text-primary mb-2 text-2xl font-medium">
      {$t`Signing in securely`}
    </p>
    <a
      href={window.location.origin}
      target="_blank"
      rel="noopener noreferrer"
      class="text-text-secondary flex flex-row items-center gap-2 text-base"
    >
      <span>{$t`Powered by`}</span>
      <Logo class="text-text-secondary h-2" />
      <span>{window.location.hostname}</span>
    </a>
  </div>
  {#await waitFor(10000) then _}
    <Dialog>
      <FeaturedIcon size="lg" class="mb-4 self-start">
        <CircleAlertIcon class="size-6" />
      </FeaturedIcon>
      <h1 class="text-text-primary mb-3 text-2xl font-medium">
        {$t`Authentication successful`}
      </h1>
      <p class="text-text-tertiary mb-6 text-base font-medium">
        {$t`You may close this page.`}
      </p>
      <button class="btn btn-secondary" onclick={() => window.close()}>
        <RotateCcwIcon class="size-4" />
        <span>{$t`Return to app`}</span>
      </button>
    </Dialog>
  {/await}
</div>

<BreatheSparkleCanvas fadeCenterHold="30%" />

<style>
  @keyframes breathe {
    0%,
    100% {
      opacity: 1;
    }
    50% {
      opacity: 0.15;
    }
  }

  .breathe {
    animation: breathe 3s ease-in-out 6.5s infinite;
  }
</style>
