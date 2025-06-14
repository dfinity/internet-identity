<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Logo from "$lib/components/ui/Logo.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
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
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 5),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);

  let identityButtonRef = $state<HTMLElement>();
  let isIdentityPopoverOpen = $state(false);
  let isSignInDialogOpen = $state(false);
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
    {#if nonNullish(selectedIdentity)}
      <Button
        bind:element={identityButtonRef}
        onclick={() => (isIdentityPopoverOpen = true)}
        variant="tertiary"
        class="ml-auto gap-2.5 pr-3 md:-mr-3"
      >
        <span>{selectedIdentity.name ?? selectedIdentity.identityNumber}</span>
        <ChevronDownIcon size="1rem" />
      </Button>

      {#if isIdentityPopoverOpen}
        <Popover
          anchor={identityButtonRef}
          onClose={() => (isIdentityPopoverOpen = false)}
          direction="down"
          align="end"
          distance="0.75rem"
        >
          <IdentitySwitcher
            selected={selectedIdentity.identityNumber}
            identities={lastUsedIdentities}
            switchIdentity={(identityNumber) => {
              authenticationStore.reset();
              lastUsedIdentitiesStore.selectIdentity(identityNumber);
              isIdentityPopoverOpen = false;
            }}
            useAnotherIdentity={() => (isSignInDialogOpen = true)}
            onClose={() => (isIdentityPopoverOpen = false)}
          />
        </Popover>
      {/if}

      {#if isSignInDialogOpen}
        <Dialog onClose={() => (isSignInDialogOpen = false)}>
          <UseAnotherIdentity
            onCancel={() => (isSignInDialogOpen = false)}
            onSuccess={(identityNumber) => {
              isSignInDialogOpen = false;
              lastUsedIdentitiesStore.selectIdentity(identityNumber);
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
