<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Radiobox from "$lib/components/ui/Radiobox.svelte";

  interface Props {
    locales: string[];
    value: string;
    onChange: (value: string) => void;
  }

  const { locales, value, onChange }: Props = $props();

  let selected = $derived(value);

  $effect(() => {
    if (value === selected) {
      return;
    }
    onChange(selected);
  });
</script>

<h2 class="text-text-primary mb-3 text-2xl">
  {$t`Language`}
</h2>
<p class="text-text-tertiary mb-6 text-base">
  <Trans>choose language to continue</Trans>
</p>
{#each locales as locale, index}
  {@const label = new Intl.DisplayNames([locale], {
    type: "language",
    style: "long",
  }).of(locale)}
  {@const hint = new Intl.DisplayNames([selected], {
    type: "language",
    style: "long",
  }).of(locale)}
  <div
    class={[
      index > 0 && "pt-3",
      index < locales.length - 1 && "border-border-secondary border-b-1 pb-3",
    ]}
  >
    <Radiobox
      {label}
      {hint}
      bind:group={selected}
      value={locale}
      class="!w-full"
    />
  </div>
{/each}
