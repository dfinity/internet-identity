<script lang="ts">
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import { originLabel } from "$lib/utils/urlUtils";
  import AppLogo from "./AppLogo.svelte";
  import { appUrl } from "../apps";

  interface Props {
    origin: string;
  }

  const { origin }: Props = $props();

  const metadataStore = $derived(getAppMetadataStore(origin));
  const metadata = $derived($metadataStore);
  const label = $derived(originLabel(appUrl(origin)));
</script>

<AppLogo logo={metadata.logo} size="card" />
<span class="flex min-w-0 flex-col">
  <span class="text-text-primary truncate text-[15px] leading-5 font-semibold">
    {metadata.name ?? label}
  </span>
  <span class="text-text-tertiary truncate text-[13px] leading-[18px]">
    {label}
  </span>
</span>
