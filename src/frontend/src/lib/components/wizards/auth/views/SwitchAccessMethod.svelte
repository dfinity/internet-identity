<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import { UserIcon } from "@lucide/svelte";

  export type AccessMethod =
    | { type: "passkey" }
    | { type: "openid"; logo: string; name: string }
    | { type: "sso"; name: string };

  interface Props {
    userName: string;
    userEmail?: string;
    fromMethod: AccessMethod;
    toMethod: AccessMethod;
    onSwitch: () => void;
  }

  let { userName, userEmail, fromMethod, toMethod, onSwitch }: Props = $props();

  const methodLabel = (m: AccessMethod): string =>
    m.type === "passkey" ? $t`Passkey` : m.name;
</script>

<div class="flex flex-col">
  <div class="flex flex-col items-start">
    <h2
      class="text-text-primary text-[22px] leading-[26.4px] font-medium tracking-tight"
    >
      {$t`Switch access method`}
    </h2>
    <p class="text-text-tertiary mt-2 max-w-80 text-sm leading-5">
      {$t`This account is already attached to your identity. Your active access method will be switched to it.`}
    </p>
  </div>

  <div class="mt-4 flex flex-col items-center py-5">
    <div class="flex items-center gap-3">
      {#each [fromMethod, toMethod] as method, i (i)}
        <span class="relative inline-block size-14 shrink-0">
          <span
            class="bg-bg-primary border-border-secondary text-fg-disabled flex size-14 items-center justify-center rounded-full border"
          >
            <UserIcon class="size-6" aria-hidden="true" />
          </span>
          <span
            class="bg-bg-primary_alt border-border-secondary text-fg-tertiary absolute -inset-e-1 -bottom-1 flex size-7 items-center justify-center rounded-full border"
          >
            {#if method.type === "passkey"}
              <PasskeyIcon class="size-4!" aria-hidden="true" />
            {:else if method.type === "openid"}
              <span class="size-4">
                <!-- eslint-disable-next-line svelte/no-at-html-tags -- method.logo is a trusted SVG string from openid_configs -->
                {@html method.logo}
              </span>
            {:else}
              <SsoIcon class="size-4!" aria-hidden="true" />
            {/if}
          </span>
        </span>
      {/each}
    </div>
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
      {methodLabel(fromMethod)} → {methodLabel(toMethod)}
    </span>
  </div>

  <button onclick={onSwitch} class="btn btn-primary btn-lg mt-3 w-full">
    {$t`Switch method`}
  </button>
</div>
