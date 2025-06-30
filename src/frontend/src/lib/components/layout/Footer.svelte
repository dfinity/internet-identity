<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";
  import Button from "$lib/components/ui/Button.svelte";
  import { goto } from "$app/navigation";
  import { ENABLE_MIGRATE_FLOW } from "$lib/state/featureFlags";
  import { page } from "$app/state";

  type Props = HTMLAttributes<HTMLElement>;

  const { children, class: className, ...props }: Props = $props();

  // TODO: Remove once we have the proper UX for triggering the migration
  const triggerMigration = () => {
    const migrationPath = page.route.id?.includes("authorize")
      ? "/authorize/migrate"
      : "/migrate";
    goto(migrationPath);
  };
</script>

<footer
  {...props}
  class={[
    "flex h-14 items-center justify-between px-4 py-3 md:px-6 lg:px-8",
    className,
  ]}
>
  <div class="text-text-tertiary text-xs font-medium">© Internet Identity</div>
  <!-- TODO: Remove once we have the proper UX for triggering the migration -->
  {#if $ENABLE_MIGRATE_FLOW}
    <Button variant="primary" onclick={triggerMigration}>Migrate</Button>
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
