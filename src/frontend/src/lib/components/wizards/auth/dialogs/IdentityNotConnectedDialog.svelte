<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { ChevronDownIcon, UserIcon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    providerName: string;
    providerLogo?: string;
    userName: string;
    userEmail?: string;
    onSignUp: () => void;
    onRecover: () => void;
    loading?: boolean;
  }

  let {
    providerName,
    providerLogo,
    userName,
    userEmail,
    onSignUp,
    onRecover,
    loading = false,
  }: Props = $props();
</script>

<div class="flex flex-col">
  <div class="flex flex-col items-start">
    <h2
      class="text-text-primary text-[22px] leading-[26.4px] font-medium tracking-tight"
    >
      {$t`Create your Identity`}
    </h2>
    <p class="text-text-tertiary mt-2 max-w-80 text-sm leading-5">
      {$t`This ${providerName} account isn't connected to an Internet Identity yet.`}
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
      class="bg-bg-primary border-border-secondary text-text-tertiary mt-2 inline-flex rounded-full border px-2.5 py-1 text-xs font-semibold"
    >
      {$t`Not connected yet`}
    </span>
  </div>

  <button
    onclick={onSignUp}
    disabled={loading}
    class="btn btn-primary btn-lg mt-3 w-full gap-2"
  >
    {#if loading}
      <ProgressRing class="size-5" />
    {/if}
    {$t`Sign up`}
  </button>

  <details class="group border-border-secondary mt-4 border-t pt-4">
    <summary
      class="text-text-tertiary flex cursor-pointer list-none items-center justify-between text-sm font-semibold outline-none"
    >
      <span>{$t`Why am I seeing this?`}</span>
      <ChevronDownIcon
        class="size-4 transition-transform group-open:rotate-180"
        aria-hidden="true"
      />
    </summary>
    <div class="text-text-tertiary mt-3 text-sm leading-5">
      <p>{$t`A few things could be going on:`}</p>
      <ul class="mt-2 list-disc ps-4">
        <li>
          {$t`You've never used this ${providerName} account with Internet Identity.`}
        </li>
        <li>
          {$t`You signed up before with a different access method.`}
        </li>
        <li>
          {$t`You unlinked this ${providerName} account from your identity.`}
        </li>
      </ul>
      <div
        class="border-border-secondary mt-3 flex items-center justify-between border-t pt-2.5"
      >
        <span>{$t`Lost access to your identity?`}</span>
        <button
          onclick={onRecover}
          class="text-text-primary font-semibold outline-0 hover:underline focus-visible:underline"
        >
          {$t`Recover`}
        </button>
      </div>
    </div>
  </details>
</div>
