<script lang="ts">
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { t } from "$lib/stores/locale.store";
  import { draw, fade, scale } from "svelte/transition";
  import { cubicOut } from "svelte/easing";
  import { GlobeIcon } from "@lucide/svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import BreatheSparkleCanvas from "$lib/components/backgrounds/BreatheSparkleCanvas.svelte";

  interface Props {
    origin: string;
  }

  const { origin }: Props = $props();

  const dapps = getDapps();
  const dapp = $derived(dapps.find((entry) => entry.hasOrigin(origin)));
  const dappName = $derived(dapp?.name ?? new URL(origin).hostname);
</script>

<div class="flex min-h-[100dvh] flex-col items-center justify-center px-8">
  <div
    in:scale={{ duration: 500, easing: cubicOut, start: 0.9 }}
    class="flex flex-col items-center justify-center"
  >
    {#if dapp?.logoSrc !== undefined}
      <div class="relative mb-4">
        <svg viewBox="0 0 92 92" width="92" height="92" class="block">
          <path
            d="M 46 1 H 71 A 20 20 0 0 1 91 21 V 71 A 20 20 0 0 1 71 91 H 21 A 20 20 0 0 1 1 71 V 21 A 20 20 0 0 1 21 1 H 46"
            class="stroke-fg-primary/10 fill-none stroke-2"
          />
          <g in:fade|global={{ duration: 500, easing: cubicOut, delay: 300 }}>
            <path
              in:draw|global={{ duration: 2000, easing: cubicOut, delay: 300 }}
              d="M 46 1 H 71 A 20 20 0 0 1 91 21 V 71 A 20 20 0 0 1 71 91 H 21 A 20 20 0 0 1 1 71 V 21 A 20 20 0 0 1 21 1 H 46"
              stroke-linecap="round"
              class="stroke-fg-primary fill-none stroke-2"
            />
          </g>
        </svg>
        <img
          src={dapp.logoSrc}
          alt={$t`${dappName} logo`}
          class="absolute inset-1 size-[84px] rounded-[18px] object-cover"
        />
      </div>
    {:else}
      <div class="relative mb-4">
        <svg viewBox="0 0 92 92" width="92" height="92" class="block">
          <circle
            cx="46"
            cy="46"
            r="45"
            class="stroke-fg-primary/10 fill-none stroke-2"
          />
          <g in:fade|global={{ duration: 500, easing: cubicOut, delay: 300 }}>
            <circle
              in:draw|global={{ duration: 2000, easing: cubicOut, delay: 300 }}
              cx="46"
              cy="46"
              r="45"
              stroke-linecap="round"
              class="stroke-fg-primary origin-center -rotate-90 fill-none stroke-2"
            />
          </g>
        </svg>
        <span
          class="text-fg-secondary absolute inset-0 flex items-center justify-center"
          aria-hidden="true"
        >
          <GlobeIcon class="size-8" />
        </span>
      </div>
    {/if}
    <p class="text-text-primary mb-2 text-center text-2xl font-medium">
      {$t`Opening ${dappName}`}
    </p>
    <div class="text-text-secondary flex flex-row items-center gap-2 text-base">
      <span>{$t`Powered by`}</span>
      <Logo class="text-text-secondary h-2" />
      <span>{new URL(window.location.href).hostname}</span>
    </div>
  </div>
</div>

<BreatheSparkleCanvas fadeCenterHold="30%" />
