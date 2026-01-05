<script lang="ts">
  import type { LayoutProps } from "../$types";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { fly, scale } from "svelte/transition";
  import { nonNullish } from "@dfinity/utils";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import MigrationIllustration from "$lib/components/illustrations/MigrationIllustration.svelte";
  import { XIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { authorizationContextStore } from "$lib/stores/authorization.store";
  import { Trans } from "$lib/components/locale";
  import { MigrationWizard } from "$lib/components/wizards/migration";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { GUIDED_UPGRADE } from "$lib/state/featureFlags";

  const { children }: LayoutProps = $props();

  let animationWrapperRef = $state<HTMLElement>();
  let isUpgradeCollapsed = $state(
    localStorage.getItem("ii-guided-upgrade-collapsed") === "true",
  );
  let upgradePanelHeight = $state<number>(224);
  let isUpgrading = $state(false);

  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );

  const onUpgrade = (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    toaster.success({
      title: $t`Upgrade completed successfully`,
      duration: 4000,
    });
    isUpgrading = false;
    isUpgradeCollapsed = true;
  };

  $effect(() => {
    localStorage.setItem(
      "ii-guided-upgrade-collapsed",
      `${isUpgradeCollapsed}`,
    );
  });
</script>

{#snippet upgradePanel()}
  <div
    class={[
      "relative overflow-hidden rounded-xl border-1 transition-all duration-200 max-sm:mx-4",
      "border-[#d3d8e6] bg-[#e8e9f2]",
      "dark:border-[#34384b] dark:bg-[#1f212d]",
      isUpgradeCollapsed && "sm:-mb-[calc(var(--upgrade-card-height)-22px)]",
    ]}
    style="--upgrade-card-height: {upgradePanelHeight}px"
  >
    <div
      bind:clientHeight={upgradePanelHeight}
      inert={isUpgradeCollapsed}
      class={[
        "relative flex flex-col items-stretch p-6 transition-all transition-discrete duration-200",
        isUpgradeCollapsed
          ? "opacity-0 max-sm:-mb-[calc(var(--upgrade-card-height)-44px)]"
          : "z-1 delay-100",
      ]}
    >
      <MigrationIllustration
        class={[
          "mb-3 self-center text-black dark:text-white",
          "!gap-3 !py-1.5 [&_svg:first-child]:!h-3 [&_svg:last-child]:!h-3 [&_svg:nth-child(2)]:!size-3",
        ]}
      />
      <h2
        class="mx-6 mb-3 text-center text-lg font-medium text-balance text-black dark:text-white"
      >
        {#if dapp?.name !== undefined}
          {@const application = dapp.name}
          {$t`${application} has moved to the new Internet Identity`}
        {:else}
          {$t`This app has moved to the new Internet Identity`}
        {/if}
      </h2>
      <p class="mb-3 text-center text-sm text-black/70 dark:text-white/70">
        <Trans>Still using an identity number?</Trans>
      </p>
      <button
        onclick={() => (isUpgrading = true)}
        class="btn !bg-[#6752cc] !text-white hover:!bg-[#6d57cf]"
      >
        {$t`Upgrade your identity`}
      </button>
      <button
        onclick={() => (isUpgradeCollapsed = true)}
        class="btn btn-tertiary btn-lg btn-icon absolute end-2 top-2 !rounded-full hover:!bg-black/5 dark:hover:!bg-white/3"
        aria-label={$t`Close`}
      >
        <XIcon class="size-5" aria-hidden="true" />
      </button>
    </div>
    <div
      inert={!isUpgradeCollapsed}
      class={[
        "absolute inset-x-0 top-0 flex h-11 flex-row items-stretch gap-3 transition-all transition-discrete duration-200",
        !isUpgradeCollapsed ? "opacity-0" : "z-1 delay-200",
      ]}
    >
      <button
        onclick={() => (isUpgradeCollapsed = false)}
        class="flex-1 border-none ps-6 text-start text-sm text-black/70 outline-none dark:text-white/70"
      >
        <Trans>Still using an identity number?</Trans>
      </button>
      <button
        onclick={() => (isUpgrading = true)}
        class="me-6 border-none text-sm font-semibold text-black/70 outline-none hover:underline focus-visible:underline dark:text-white/70"
      >
        {$t`Upgrade`}
      </button>
    </div>
  </div>
  {#if isUpgrading}
    <Dialog onClose={() => (isUpgrading = false)}>
      <MigrationWizard
        onSuccess={onUpgrade}
        onError={(error) => {
          handleError(error);
          isUpgrading = false;
        }}
      />
    </Dialog>
  {/if}
{/snippet}

<div class="grid w-full flex-1 items-center max-sm:items-stretch sm:max-w-100">
  {#if nonNullish(selectedIdentity)}
    {#key selectedIdentity.identityNumber}
      <div
        bind:this={animationWrapperRef}
        class="relative col-start-1 row-start-1 flex flex-col gap-5"
        in:fly={{ duration: 300, y: 60, delay: 200 }}
        out:scale={{ duration: 500, start: 0.9 }}
        onoutrostart={() =>
          animationWrapperRef?.setAttribute("aria-hidden", "true")}
      >
        {#if $GUIDED_UPGRADE}
          {@render upgradePanel()}
        {/if}
        <AuthPanel
          class={[
            "z-1",
            $GUIDED_UPGRADE && isUpgradeCollapsed && "rounded-t-none",
          ]}
        >
          {@render children()}
        </AuthPanel>
      </div>
    {/key}
  {:else}
    <div class="relative col-start-1 row-start-1 flex flex-col gap-5">
      {#if $GUIDED_UPGRADE}
        {@render upgradePanel()}
      {/if}
      <AuthPanel
        class={[
          "z-1",
          $GUIDED_UPGRADE && isUpgradeCollapsed && "rounded-t-none",
        ]}
      >
        {@render children()}
      </AuthPanel>
    </div>
  {/if}
</div>
