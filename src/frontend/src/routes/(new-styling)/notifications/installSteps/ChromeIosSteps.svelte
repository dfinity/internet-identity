<script lang="ts">
  import FocusIcon from "@lucide/svelte/icons/focus";
  import PrinterIcon from "@lucide/svelte/icons/printer";
  import ShareIcon from "@lucide/svelte/icons/share";
  import SquarePlusIcon from "@lucide/svelte/icons/square-plus";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Logo from "$lib/components/ui/Logo.svelte";
  import { NOTIFICATION_APP_NAME } from "../appName";
  import ControlCircle from "$lib/components/ui/browserMock/ControlCircle.svelte";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import QuickNoteIcon from "./icons/QuickNoteIcon.svelte";
  import ConfirmSheet from "./ConfirmSheet.svelte";
  import HomeScreenGrid from "./HomeScreenGrid.svelte";
  import ViewMoreSheet from "./ViewMoreSheet.svelte";

  const { host }: { host: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Share the page`}>
    {#snippet instruction()}
      <Trans>Tap the highlighted <b>share</b> icon.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-9 items-center gap-2 rounded-full pr-1 pl-3"
      >
        <span class="flex opacity-45"
          ><FocusIcon class="size-3.5 shrink-0" /></span
        >
        <span class="flex-1 text-center text-[10px]">{host}</span>
        <ToolbarHighlight>
          <ControlCircle class="h-7 w-7"
            ><ShareIcon class="size-3.5 shrink-0" /></ControlCircle
          >
        </ToolbarHighlight>
      </div>
    </Surface>
  </Step>

  <Step n={2} title={$t`Show more options`}>
    {#snippet instruction()}
      <Trans>Tap <b>View More</b>.</Trans>
    {/snippet}
    <ViewMoreSheet />
  </Step>

  <Step n={3} title={$t`Add to Home Screen`}>
    {#snippet instruction()}
      <Trans>Tap <b>Add to Home Screen</b>.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <PrinterIcon class="size-[13px] shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Print`}</span>
      </div>
      <ToolbarHighlight class="mt-1.5 mb-1">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-8 items-center gap-2.5 rounded-full px-2.5"
        >
          <SquarePlusIcon class="size-[13px] shrink-0" />
          <span class="flex-1 text-[11px]">{$t`Add to Home Screen`}</span>
        </div>
      </ToolbarHighlight>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <QuickNoteIcon width="13" height="13" class="shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Add to New Quick Note`}</span>
      </div>
    </Surface>
  </Step>

  <Step n={4} title={$t`Confirm`}>
    {#snippet instruction()}
      <Trans>Keep <b>Open as Web App</b> on, then tap <b>Add</b>.</Trans>
    {/snippet}
    <ConfirmSheet />
  </Step>

  <Step n={5} title={$t`Open the app`}>
    {#snippet instruction()}
      <Trans>Open it from your <b>Home Screen</b>.</Trans>
    {/snippet}
    <HomeScreenGrid label={NOTIFICATION_APP_NAME} blanks={6}>
      {#snippet icon()}<Logo
          width="24"
          height="12"
          class="shrink-0"
        />{/snippet}
    </HomeScreenGrid>
  </Step>
</Steps>
