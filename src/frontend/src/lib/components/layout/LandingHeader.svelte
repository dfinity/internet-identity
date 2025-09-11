<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Logo from "$lib/components/ui/Logo.svelte";

  import { locale } from "$lib/utils/translations";
  import langMap from "$lib/utils/translations/lang.json";
  type Props = HTMLAttributes<HTMLElement>;
  const { children, class: className, ...props }: Props = $props();

  const languages = Object.entries(langMap);
  const switchLocale = (lang: string) => locale.set(lang);
</script>

<header
  {...props}
  class={["bg-bg-primary flex items-center px-0 md:px-6 lg:px-8", className]}
>
  <div class="flex min-h-16 flex-1 items-center gap-4">
    <Logo class="text-fg-primary h-5.5" />
    <h1 class="text-md text-text-primary hidden font-semibold sm:block">
      Internet Identity
    </h1>
  </div>
  <select
    class="bg-bg-primary text-text-primary"
    onchange={(e) => switchLocale((e.target as HTMLSelectElement).value)}
    bind:value={$locale}
  >
    {#each languages as [code, label]}
      <option value={code}>{label}</option>
    {/each}
  </select>
  {@render children?.()}
</header>
