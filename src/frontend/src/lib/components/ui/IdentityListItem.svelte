<script lang="ts">
  import { formatRelative } from "$lib/stores/locale.store";
  import { getMetadataString } from "$lib/utils/openID";
  import Avatar from "./Avatar.svelte";

  type Props = {
    identity: LastUsedIdentity;
    notUniqueName?: boolean;
  };
  let { identity, notUniqueName }: Props = $props();
  const logo = $derived(
    "openid" in identity.authMethod &&
      identity.authMethod.openid.metadata !== undefined
      ? openIdLogo(
          identity.authMethod.openid.iss,
          identity.authMethod.openid.metadata,
        )
      : undefined,
  );
</script>

{#snippet authMethodBadge(logo: string | undefined, size: "sm" | "lg")}
  <span
    class={[
      "bg-bg-primary_alt border-border-secondary absolute flex items-center justify-center rounded-full border",
      size === "lg"
        ? "-inset-e-1 -bottom-1 size-6.5"
        : "-inset-e-1.25 -bottom-1.25 size-5",
    ]}
  >
    {#if logo !== undefined}
      <span
        class={["text-fg-tertiary", size === "lg" ? "size-4.25" : "size-3.25"]}
      >
        {@html logo}
      </span>
    {:else}
      <PasskeyIcon
        class={["text-fg-tertiary", size === "lg" ? "size-4.25!" : "size-3!"]}
      />
    {/if}
  </span>
{/snippet}

<div class="flex w-full flex-row items-center gap-3">
  <span class="relative">
    <Avatar size="sm">
      <UserIcon class="size-5" />
    </Avatar>
    {@render authMethodBadge(logo, "sm")}
  </span>
  <span class="flex flex-col overflow-hidden group-disabled:opacity-50">
    <span class="text-text-primary text-sm font-semibold">
      {identity.name ?? identity.identityNumber}
    </span>
    <span
      class="text-text-tertiary overflow-hidden text-sm text-ellipsis whitespace-nowrap"
    >
      {#if "openid" in identity.authMethod && identity.authMethod.openid.metadata !== undefined}
        <span
          >{getMetadataString(identity.authMethod.openid.metadata, "email") ??
            $t`Hidden email`}</span
        >
      {:else}
        <span>
          {$t`Passkey`}
          {#if notUnique && identity.createdAtMillis !== undefined}
            {" | "}
            {$t`Created ${$formatRelative(new Date(identity.createdAtMillis), {
              style: "long",
            })}`}
          {/if}
        </span>
      {/if}
    </span>
  </span>
</div>
