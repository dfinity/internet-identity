<script lang="ts">
  import Panel from "$lib/components/ui/Panel.svelte";
  import { ChevronRightIcon, KeyRoundIcon, LockIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    totalAccessMethods: number;
  }

  const { totalAccessMethods }: Props = $props();
</script>

<h2 class="text-text-primary mb-2 text-lg font-semibold">
  {$t`Access methods`}
</h2>
<p class="text-text-tertiary mb-auto text-sm">
  <Trans>
    See how many access methods are within your identity and manage your
    recovery phrase.
  </Trans>
</p>
<div class="border-border-tertiary my-5 border-t"></div>
<div class="text-text-tertiary mb-3 text-sm">
  {$t`Number of access methods`}
</div>
<div class="flex flex-row items-center gap-3">
  <KeyRoundIcon class="text-fg-secondary size-6" />
  <span class="text-text-primary text-sm font-semibold">
    {totalAccessMethods}
  </span>
  <Button
    href="/manage/access"
    variant="tertiary"
    iconOnly
    class="ms-auto !rounded-full"
  >
    <ChevronRightIcon class="size-5" />
  </Button>
</div>
