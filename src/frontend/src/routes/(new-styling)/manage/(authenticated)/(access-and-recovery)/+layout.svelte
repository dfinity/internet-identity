<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { t } from "$lib/stores/locale.store";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { page } from "$app/state";
  import { toAccessMethods } from "./access/utils";
  import { getMetadataString } from "$lib/utils/openID";

  const { children, data }: LayoutProps = $props();

  const accessMethodCount = $derived(toAccessMethods(data.identityInfo).length);

  const hasRecoveryPhrase = $derived(
    data.identityInfo.authn_methods.some(
      (m) =>
        "Recovery" in m.security_settings.purpose &&
        getMetadataString(m.metadata, "usage") === "recovery_phrase",
    ),
  );
  const hasRecoveryEmail = $derived(
    data.identityInfo.email_recovery[0] !== undefined,
  );
  const recoveryMethodCount = $derived(
    (hasRecoveryPhrase ? 1 : 0) + (hasRecoveryEmail ? 1 : 0),
  );
</script>

<nav class="tabs mb-8">
  <a
    href="/manage/access"
    class={["tab", page.url.pathname === "/manage/access" && "tab-selected"]}
  >
    <span>{$t`Access methods`}</span>
    <Badge size="sm">{accessMethodCount}</Badge>
  </a>
  <a
    href="/manage/recovery"
    class={["tab", page.url.pathname === "/manage/recovery" && "tab-selected"]}
  >
    <span>{$t`Recovery methods`}</span>
    {#if recoveryMethodCount > 0}
      <Badge size="sm">{recoveryMethodCount}</Badge>
    {/if}
  </a>
</nav>

{@render children()}
