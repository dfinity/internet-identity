<script lang="ts">
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { InfoIcon, LockIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    name: string;
  }

  const { name }: Props = $props();
</script>

<h2 class="text-text-primary mb-2 text-lg font-semibold">{$t`My identity`}</h2>
<p class="text-text-tertiary mb-auto text-sm">
  <Trans>
    Internet Identity is used to sign in securely and connect to apps with
    passkeys.
  </Trans>
</p>
<div class="border-border-tertiary my-5 border-t"></div>
<div class="text-text-tertiary mb-3 text-sm">{$t`Identity name`}</div>
<div class="flex flex-row items-center gap-3">
  <LockIcon class="text-fg-secondary size-6" />
  <span class="text-text-primary text-sm font-semibold">{name}</span>
  <Tooltip
    label={$t`Your identity name is currently not editable. It is only ever visible to you.`}
    align="end"
    offset="0rem"
    class="!max-w-72"
  >
    <Button
      variant="tertiary"
      iconOnly
      class="ms-auto !cursor-default !rounded-full"
    >
      <InfoIcon class="size-5" />
    </Button>
  </Tooltip>
</div>
