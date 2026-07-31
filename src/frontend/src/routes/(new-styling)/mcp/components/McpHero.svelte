<script lang="ts">
  import Logo from "$lib/components/ui/Logo.svelte";
  import { BotIcon } from "@lucide/svelte";

  const connectorDots = [
    "opacity-30",
    "opacity-50",
    "opacity-70",
    "opacity-50",
    "opacity-30",
  ];
</script>

<!--
  The AI agent joined to Internet Identity: the two ends of the connection being
  granted. The server it belongs to is named in each screen's own copy, so this
  carries no text.
-->
<div
  class="flex flex-1 items-center justify-center gap-3 py-2"
  aria-hidden="true"
>
  <div
    class="border-border-tertiary text-fg-primary bg-bg-primary flex size-14 shrink-0 items-center justify-center rounded-xl border"
  >
    <BotIcon class="size-6" />
  </div>
  <div class="flex items-center gap-1.5">
    {#each connectorDots as dotOpacity, index (index)}
      <span class={["bg-fg-primary size-1 rounded-full", dotOpacity]}></span>
    {/each}
  </div>
  <div
    class="border-border-tertiary text-fg-primary bg-bg-primary flex size-14 shrink-0 items-center justify-center rounded-xl border"
  >
    <Logo class="h-4" />
  </div>
</div>
