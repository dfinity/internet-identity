<script lang="ts">
  import { ArrowLeftIcon } from "@lucide/svelte";

  interface Props {
    title: string;
    lead: string;
    backLabel?: string;
    onBack: () => void;
  }

  const { title, lead, backLabel = "Back", onBack }: Props = $props();
</script>

<header class="flex items-center gap-3">
  <button
    type="button"
    onclick={onBack}
    class="btn btn-secondary btn-icon btn-md shrink-0 rounded-xl"
  >
    <ArrowLeftIcon class="size-4.5 rtl:-scale-x-100" />
    <span>{backLabel}</span>
  </button>
  <div class="flex min-w-0 flex-col">
    <h2 class="text-text-primary text-sm font-medium">
      {title}
    </h2>
    <p class="text-text-tertiary text-[13px] leading-[18px]">{lead}</p>
  </div>
</header>
