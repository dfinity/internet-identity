<!--
  The ways in, as the product's picker (`PickAuthenticationMethod`) offers
  them on today's landing page, in its two modes:

  - Sign in to an identity you have: a passkey first, then the providers
    side by side, then the two ways in from elsewhere (from another device,
    by a link or QR code; and recovery, for a lost identity), and last the
    way to create a new one.
  - Create one: the same buttons, now creating, and the way back to
    signing in.

  Here on the concept's surfaces: the providers stand a step above what
  they lie on, and the buttons are the concept's pills. `mode` is bindable,
  so the heading above can follow it.
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
    /** A way picked: what it's called, and what would happen next. */
    onPick: (label: string, next?: string) => void;
    mode?: "signin" | "signup";
    /** Drops the lines under the buttons, where the layout has its own. */
    bare?: boolean;
    class?: ClassValue;
  }

  let {
    onPick,
    mode = $bindable("signin"),
    bare = false,
    class: className,
  }: Props = $props();

  const raised = surfaceColor(useSurface() + 1);
  const verb = $derived(mode === "signin" ? "Continue" : "Create");

  const PROVIDERS: { id: Provider; name: string }[] = [
    { id: "google", name: "Google" },
    { id: "apple", name: "Apple" },
    { id: "microsoft", name: "Microsoft" },
    { id: "sso", name: "SSO" },
  ];

  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
  const LINK =
    "text-text-primary inline-flex shrink-0 items-center gap-1 font-medium outline-none hover:underline focus-visible:underline";
</script>

<div class={["flex flex-col gap-6", className]}>
  <div class="flex flex-col gap-3">
    <button
      type="button"
      onclick={() => onPick(`${verb} with passkey`)}
      class={["btn btn-primary btn-xl h-14 rounded-full font-medium", PRESS]}
    >
      <ProviderLogo provider="passkey" />
      {verb} with passkey
    </button>
    <div class="grid grid-cols-4 gap-2">
      {#each PROVIDERS as provider (provider.id)}
        <button
          type="button"
          onclick={() => onPick(`${verb} with ${provider.name}`)}
          aria-label={`${verb} with ${provider.name}`}
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
  {#if mode === "signin" && !bare}
    <div class="flex flex-col gap-3 text-sm">
      <div class="flex items-center justify-between gap-4">
        <span class="text-text-tertiary">Add identity from another device</span>
        <button
          type="button"
          onclick={() =>
            onPick(
              "Add identity from another device",
              "Here a link and a QR code open, to approve this device from one that has the identity.",
            )}
          class={LINK}
        >
          Link or QR code
        </button>
      </div>
      <div class="flex items-center justify-between gap-4">
        <span class="text-text-tertiary">Lost access to your identity?</span>
        <button
          type="button"
          onclick={() =>
            onPick(
              "Recover an identity",
              "Here recovery opens, with your recovery phrase or email.",
            )}
          class={LINK}
        >
          Recover
          <ArrowRightIcon
            class="size-3.5 rtl:-scale-x-100"
            aria-hidden="true"
          />
        </button>
      </div>
    </div>
  {/if}
  <div class="border-border-tertiary flex items-center gap-4 border-t pt-5">
    <div class="flex min-w-0 flex-1 flex-col gap-0.5">
      <span class="text-text-primary text-sm font-medium">
        {mode === "signin"
          ? "Create new identity"
          : "Already have an identity?"}
      </span>
      <span class="text-text-tertiary text-xs">
        Use a passkey or familiar provider.
      </span>
    </div>
    <button
      type="button"
      onclick={() => (mode = mode === "signin" ? "signup" : "signin")}
      class={["btn btn-secondary btn-sm shrink-0 rounded-full", PRESS]}
    >
      {mode === "signin" ? "Create" : "Sign in"}
      <ArrowRightIcon class="size-3.5 rtl:-scale-x-100" aria-hidden="true" />
    </button>
  </div>
</div>
