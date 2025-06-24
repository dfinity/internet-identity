<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";
  import Button from "../ui/Button.svelte";
  import { goto } from "$app/navigation";
  import { ENABLE_MIGRATE_FLOW } from "$lib/state/featureFlags";

  type Props = HTMLAttributes<HTMLElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<footer
  {...props}
  class={[
    "flex h-14 items-center justify-between px-4 py-3 md:px-6 lg:px-8",
    className,
  ]}
>
  <div class="text-text-tertiary text-xs font-medium">© Internet Identity</div>
  {#if $ENABLE_MIGRATE_FLOW}
    <Button variant="primary" onclick={() => goto("/migrate")}>Migrate</Button>
  {/if}
  <nav class="text-text-primary flex gap-4 text-xs font-semibold">
    <a
      href={SUPPORT_URL}
      target="_blank"
      rel="noopener"
      class="outline-0 focus-visible:underline"
    >
      Support
    </a>
    <a
      href={SOURCE_CODE_URL}
      target="_blank"
      rel="noopener"
      class="outline-0 focus-visible:underline"
    >
      Source code
    </a>
  </nav>
</footer>
