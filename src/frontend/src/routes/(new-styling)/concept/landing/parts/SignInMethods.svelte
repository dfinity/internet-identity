<!--
  The ways to sign in or to create an identity, as the product's picker
  (`PickAuthenticationMethod`, mode "both") offers them: a passkey first,
  then the providers side by side, then the way back in for a lost identity.
  Here on the concept's surfaces: the providers stand a step above what they
  lie on, and the buttons are the concept's pills.
-->
<script lang="ts">
  import { ArrowRightIcon } from "@lucide/svelte";
  import type { ClassValue } from "svelte/elements";
  import {
    surfaceColor,
    useSurface,
  } from "../../foundation/surfaces/Elevated.svelte";
  import ProviderLogo, { type Provider } from "./ProviderLogo.svelte";

  interface Props {
    onPick: (label: string) => void;
    /** Drops the lost-identity line, where the layout has its own. */
    bare?: boolean;
    class?: ClassValue;
  }

  const { onPick, bare = false, class: className }: Props = $props();

  const raised = surfaceColor(useSurface() + 1);

  const PROVIDERS: { id: Provider; name: string }[] = [
    { id: "google", name: "Google" },
    { id: "apple", name: "Apple" },
    { id: "microsoft", name: "Microsoft" },
    { id: "sso", name: "SSO" },
  ];

  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
</script>

<div class={["flex flex-col gap-6", className]}>
  <div class="flex flex-col gap-3">
    <button
      type="button"
      onclick={() => onPick("Continue with passkey")}
      class={["btn btn-primary btn-xl h-14 rounded-full font-medium", PRESS]}
    >
      <ProviderLogo provider="passkey" />
      Continue with passkey
    </button>
    <div class="grid grid-cols-4 gap-2">
      {#each PROVIDERS as provider (provider.id)}
        <button
          type="button"
          onclick={() =>
            onPick(
              provider.id === "sso"
                ? "Continue with SSO"
                : `Continue with ${provider.name}`,
            )}
          aria-label={`Continue with ${provider.name}`}
          class={[
            "ii-hover text-text-primary focus-visible:ring-focus-ring flex h-16 flex-col items-center justify-center gap-1.5 rounded-2xl text-xs font-medium shadow-(--ii-shadow-2) outline-none focus-visible:ring-2",
            PRESS,
          ]}
          style:background-color={raised}
        >
          <ProviderLogo provider={provider.id} class="text-fg-primary size-5" />
          {provider.name}
        </button>
      {/each}
    </div>
  </div>
  {#if !bare}
    <div class="flex items-center justify-between gap-4 text-sm">
      <span class="text-text-tertiary">Lost access to your identity?</span>
      <button
        type="button"
        onclick={() => onPick("Recover an identity")}
        class="text-text-primary inline-flex items-center gap-1 font-medium outline-none hover:underline focus-visible:underline"
      >
        Recover
        <ArrowRightIcon class="size-3.5 rtl:-scale-x-100" aria-hidden="true" />
      </button>
    </div>
  {/if}
</div>
