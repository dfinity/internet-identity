<!--
  The providers' marks for the sign-in buttons. In the product they come
  from the canister's OpenID config; the concept has no canister, so they
  are drawn here.
-->
<script lang="ts" module>
  export type Provider = "passkey" | "google" | "apple" | "microsoft" | "sso";
</script>

<script lang="ts">
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import type { ClassValue } from "svelte/elements";

  const {
    provider,
    class: className = "size-5",
  }: { provider: Provider; class?: ClassValue } = $props();
</script>

{#if provider === "google"}
  <svg viewBox="0 0 48 48" aria-hidden="true" class={className}>
    <path
      fill="#FFC107"
      d="M43.6 20.1H42V20H24v8h11.3C33.7 32.7 29.2 36 24 36c-6.6 0-12-5.4-12-12s5.4-12 12-12c3.1 0 5.8 1.2 8 3l5.7-5.7C34 6.1 29.3 4 24 4 13 4 4 13 4 24s9 20 20 20 20-9 20-20c0-1.3-.1-2.6-.4-3.9z"
    />
    <path
      fill="#FF3D00"
      d="m6.3 14.7 6.6 4.8C14.7 15.1 19 12 24 12c3.1 0 5.8 1.2 8 3l5.7-5.7C34 6.1 29.3 4 24 4 16.3 4 9.7 8.3 6.3 14.7z"
    />
    <path
      fill="#4CAF50"
      d="M24 44c5.2 0 9.9-2 13.4-5.2l-6.2-5.2C29.2 35.1 26.7 36 24 36c-5.2 0-9.6-3.3-11.3-7.9l-6.5 5C9.5 39.6 16.2 44 24 44z"
    />
    <path
      fill="#1976D2"
      d="M43.6 20.1H42V20H24v8h11.3c-.8 2.2-2.2 4.2-4.1 5.6l6.2 5.2C37 39.2 44 34 44 24c0-1.3-.1-2.6-.4-3.9z"
    />
  </svg>
{:else if provider === "apple"}
  <svg viewBox="0 0 24 24" aria-hidden="true" class={className}>
    <path
      class="fill-current"
      d="M12.15 6.9c-.95 0-2.42-1.08-3.96-1.04-2.04.03-3.91 1.18-4.96 3.01-2.12 3.68-.55 9.1 1.52 12.09 1.01 1.45 2.2 3.09 3.79 3.04 1.52-.07 2.09-.99 3.94-.99 1.83 0 2.35.99 3.96.95 1.64-.03 2.68-1.48 3.67-2.95 1.16-1.69 1.64-3.33 1.66-3.41-.04-.02-3.18-1.22-3.22-4.86-.03-3.04 2.48-4.49 2.6-4.56-1.43-2.09-3.62-2.32-4.39-2.38-2-.15-3.68 1.09-4.61 1.09zm3.38-3.07c.84-1.01 1.4-2.43 1.25-3.83-1.21.05-2.67.8-3.53 1.82-.78.9-1.46 2.34-1.27 3.71 1.33.11 2.71-.69 3.55-1.7"
    />
  </svg>
{:else if provider === "microsoft"}
  <svg viewBox="0 0 24 24" aria-hidden="true" class={className}>
    <path fill="#F25022" d="M2 2h9.5v9.5H2z" />
    <path fill="#7FBA00" d="M12.5 2H22v9.5h-9.5z" />
    <path fill="#00A4EF" d="M2 12.5h9.5V22H2z" />
    <path fill="#FFB900" d="M12.5 12.5H22V22h-9.5z" />
  </svg>
{:else if provider === "sso"}
  <SsoIcon class={className} />
{:else}
  <PasskeyIcon class={className} />
{/if}
