<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Logo from "$lib/components/ui/Logo.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { currentIdentityNumberStore } from "$lib/stores/current-identity.store";
  import { nonNullish } from "@dfinity/utils";
  import { ChevronDownIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { goto } from "$app/navigation";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import UseAnotherIdentity from "$lib/components/views/UseAnotherIdentity.svelte";
  import IdentitySwitcher from "$lib/components/ui/IdentitySwitcher.svelte";
  import { authenticationStore } from "$lib/stores/authentication.store";

  type Props = HTMLAttributes<HTMLHeadElement>;

  const { children, class: className, ...props }: Props = $props();

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 5),
  );
  const currentIdentity = $derived(
    nonNullish($currentIdentityNumberStore)
      ? lastUsedIdentities.find(
          (identity) => identity.identityNumber === $currentIdentityNumberStore,
        )
      : undefined,
  );

  let identityPopover = $state(false);
  let signInDialog = $state(false);
</script>

<header
  {...props}
  class={["flex items-center px-3 md:px-6 lg:px-8", className]}
>
  <!-- TODO: Revert to anchor tag when we create the landing page -->
  <div class="flex h-16 flex-1 items-center gap-4">
    <Logo class="text-fg-primary h-5.5" />
    <h1 class="text-md text-text-primary hidden font-semibold sm:block">
      Internet Identity
    </h1>
    {#if nonNullish(currentIdentity)}
      <Button
        onclick={() => (identityPopover = !identityPopover)}
        variant="tertiary"
        class="identity-switcher-button ml-auto gap-2.5 pr-3 md:-mr-3"
      >
        <span>{currentIdentity.name ?? currentIdentity.identityNumber}</span>
        <ChevronDownIcon size="1rem" />
      </Button>

      {#if identityPopover}
        <Popover
          onClose={() => (identityPopover = false)}
          class="identity-switcher-popover mt-3 origin-top-right"
        >
          <IdentitySwitcher
            currentIdentityNumber={currentIdentity.identityNumber}
            identities={lastUsedIdentities}
            switchIdentity={(identityNumber) => {
              authenticationStore.reset();
              currentIdentityNumberStore.set(identityNumber);
              identityPopover = false;
            }}
            useAnotherIdentity={() => {
              signInDialog = true;
              identityPopover = false;
            }}
            onClose={() => (identityPopover = false)}
          />
        </Popover>
      {/if}

      {#if signInDialog}
        <Dialog onClose={() => (signInDialog = false)}>
          <UseAnotherIdentity
            onCancel={() => (signInDialog = false)}
            onSuccess={() => {
              signInDialog = false;
              goto("/authorize/account", {
                replaceState: true,
                invalidateAll: true,
                state: { disableNavigationAnimation: true },
              });
            }}
          />
        </Dialog>
      {/if}
    {/if}
  </div>
</header>

<style>
  :global {
    .identity-switcher-button {
      anchor-name: --anchor-identity-switcher;
    }

    .identity-switcher-popover {
      position-anchor: --anchor-identity-switcher;
      position-area: bottom span-left;
    }
  }
</style>
