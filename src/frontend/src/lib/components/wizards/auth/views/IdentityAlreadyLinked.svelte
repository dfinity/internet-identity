<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { InfoIcon, UserIcon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    providerName: string;
    providerLogo?: string;
    userName: string;
    userEmail?: string;
    onSignIn: () => void;
    loading?: boolean;
  }

  let {
    providerName,
    providerLogo,
    userName,
    userEmail,
    onSignIn,
    loading = false,
  }: Props = $props();
</script>

<div class="flex flex-col">
  <div class="flex flex-col items-start">
    <h2
      class="text-text-primary text-[22px] leading-[26.4px] font-medium tracking-tight"
    >
      {$t`Already connected`}
    </h2>
    <p class="text-text-tertiary mt-2 max-w-80 text-sm leading-5">
      {$t`This ${providerName} account is already connected to an Internet Identity.`}
    </p>
  </div>

  <div class="mt-4 flex flex-col items-center py-5">
    <span class="relative inline-block size-16 shrink-0">
      <span
        class="bg-bg-primary border-border-secondary text-fg-disabled flex size-16 items-center justify-center rounded-full border"
      >
        <UserIcon class="size-7" aria-hidden="true" />
      </span>
      {#if providerLogo !== undefined}
        <span
          class="bg-bg-primary_alt border-border-secondary absolute -inset-e-1 -bottom-1 flex size-8 items-center justify-center rounded-full border"
        >
          <span class="size-5">
            <!-- eslint-disable-next-line svelte/no-at-html-tags -- providerLogo is a trusted SVG string from openid_configs -->
            {@html providerLogo}
          </span>
        </span>
      {/if}
    </span>
    <div class="mt-2 text-center">
      <div class="text-text-primary text-base leading-tight font-semibold">
        {userName}
      </div>
      {#if userEmail !== undefined}
        <div class="text-text-tertiary mt-0.5 text-[13px]">{userEmail}</div>
      {/if}
    </div>
    <span
      class="bg-bg-primary border-border-secondary text-text-tertiary mt-2 inline-flex items-center gap-1.5 rounded-full border px-2.5 py-1 text-xs font-semibold"
    >
      <InfoIcon class="size-3.5" aria-hidden="true" />
      {$t`Connected`}
    </span>
  </div>

  <button
    onclick={onSignIn}
    disabled={loading}
    class="btn btn-primary btn-lg mt-3 w-full gap-2"
  >
    {#if loading}
      <ProgressRing class="size-4" />
    {:else}
      <UserIcon class="size-4" aria-hidden="true" />
    {/if}
    {$t`Sign in`}
  </button>
</div>
