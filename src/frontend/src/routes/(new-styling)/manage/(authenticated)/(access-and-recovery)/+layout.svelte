<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { t } from "$lib/stores/locale.store";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { page } from "$app/state";

  const { children }: LayoutProps = $props();
</script>

<nav class="tabs mb-8">
  <a
    href="/manage/access"
    class={["tab", page.url.pathname === "/manage/access" && "tab-selected"]}
  >
    <span>{$t`Access methods`}</span>
    <Badge size="sm">2</Badge>
  </a>
  <a
    href="/manage/recovery"
    class={["tab", page.url.pathname === "/manage/recovery" && "tab-selected"]}
  >
    {$t`Recovery phrase`}
  </a>
</nav>

{@render children()}
