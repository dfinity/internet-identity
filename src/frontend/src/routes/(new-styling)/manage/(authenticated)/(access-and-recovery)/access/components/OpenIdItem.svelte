<script lang="ts">
  import { getMetadataString, openIdLogo, openIdName } from "$lib/utils/openID";
  import { EllipsisVerticalIcon, Link2OffIcon } from "@lucide/svelte";
  import { nanosToMillis } from "$lib/utils/time";
  import Select from "$lib/components/ui/Select.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { OpenIdCredential } from "$lib/generated/internet_identity_types";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";

  interface Props {
    openid: OpenIdCredential;
    onUnlink?: () => void;
    isCurrentAccessMethod?: boolean;
  }

  const { openid, onUnlink, isCurrentAccessMethod }: Props = $props();

  // `sso_domain` / `sso_name` are populated by the canister at response
  // time via `openid::generic::sso_fields_for(iss, aud)`. Candid `opt
  // text` surfaces on the TS side as `[] | [string]`, hence the `[0]`
  // unwrap.
  const ssoDomain = $derived(openid.sso_domain[0]);
  const ssoName = $derived(openid.sso_name[0]);
  // Authoritative SSO marker — set by the canister for credentials
  // whose `(iss, aud)` resolves to a registered discoverable provider.
  const isSso = $derived(ssoDomain !== undefined);
  const name = $derived(
    openIdName(openid.iss, openid.aud, openid.metadata, ssoName, ssoDomain),
  );
  const email = $derived(getMetadataString(openid.metadata, "email"));
  const logo = $derived(
    openIdLogo(openid.iss, openid.aud, openid.metadata, ssoDomain),
  );
  const displayName = $derived(name ?? $t`SSO`);
  const options = $derived(
    onUnlink !== undefined
      ? [
          {
            label: $t`Unlink`,
            icon: Link2OffIcon,
            onClick: onUnlink,
          },
        ]
      : [],
  );
</script>

<div class="mb-3 flex h-9 flex-row items-center">
  <div class="text-fg-primary relative size-6">
    {#if isSso || logo === undefined}
      <!--
        SSO credentials render the generic SSO icon. We also fall back
        to it when `findConfig` returned nothing (direct-provider
        credentials whose issuer doesn't match any `openid_configs`
        entry, e.g. after client_id rotation) so `{@html logo}` never
        stringifies `undefined` into the DOM.
      -->
      <SsoIcon class="size-6" />
    {:else}
      {@html logo}
    {/if}
    {#if isCurrentAccessMethod}
      <div
        class="bg-bg-success-secondary border-bg-primary absolute -top-0.25 -right-0.5 size-2.5 rounded-full border-2"
      ></div>
    {/if}
  </div>
  {#if options.length > 0}
    <Select {options} align="end">
      <Button
        variant="tertiary"
        size="sm"
        iconOnly
        class="ms-auto"
        aria-label={$t`More options`}
      >
        <EllipsisVerticalIcon class="size-5" />
      </Button>
    </Select>
  {/if}
</div>
<div class="text-text-primary mb-1 text-base font-semibold">
  {$t`${displayName} account`}
</div>
<div class="text-text-tertiary text-sm">
  {email ?? $t`Hidden email`}
</div>
<div class="border-border-tertiary my-5 border-t"></div>
<div class="mb-4 flex flex-row">
  <div class="flex flex-1 flex-col gap-1">
    <div class="text-text-primary text-xs font-semibold">
      {$t`Last used`}
    </div>
    <div class="text-text-primary cursor-default text-xs">
      {#if isCurrentAccessMethod}
        <Tooltip
          label={$t`Currently signed in with this account`}
          direction="up"
          align="start"
        >
          <span>{$t`Right now`}</span>
        </Tooltip>
      {:else if openid.last_usage_timestamp[0] !== undefined}
        {@const date = new Date(nanosToMillis(openid.last_usage_timestamp[0]))}
        <Tooltip
          label={$formatDate(date, {
            timeStyle: "short",
            dateStyle: "medium",
          })}
          direction="up"
          align="start"
        >
          <span>{$formatRelative(date, { style: "long" })}</span>
        </Tooltip>
      {:else}
        <Tooltip label={$t`Has not been used yet`} direction="up" align="start">
          <span>{$t`n/a`}</span>
        </Tooltip>
      {/if}
    </div>
  </div>
</div>
<div class="text-text-primary text-xs">
  {$t`Sign in with your ${displayName} account from any device.`}
</div>
