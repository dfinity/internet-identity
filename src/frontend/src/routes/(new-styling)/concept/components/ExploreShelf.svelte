<script lang="ts">
  /**
   * A shelf of apps the way the App Store lists them: icon, name, one line
   * about it, and Open. Rows stay put rather than scroll sideways, so a
   * horizontal swipe always means the next page.
   */
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { FeaturedApp } from "../mockData";

  interface Props {
    title: string;
    apps: FeaturedApp[];
  }

  const { title, apps }: Props = $props();
</script>

<section class="flex flex-col gap-2">
  <h2 class="text-text-primary px-2 text-xl font-semibold tracking-tight">
    {title}
  </h2>
  <ul class="grid gap-x-4 @2xl:grid-cols-2 @5xl:grid-cols-3">
    {#each apps as app (app.origin)}
      <li>
        <a
          href={app.url}
          target="_blank"
          rel="noopener noreferrer"
          class="hover:bg-bg-primary_hover focus-visible:outline-focus-ring flex items-center gap-3 rounded-2xl p-2 transition-colors duration-150 ease-out focus-visible:outline-2 motion-reduce:transition-none"
        >
          <AppLogo logo={app.logo} size="tile" />
          <span class="flex min-w-0 flex-1 flex-col gap-0.5">
            <span class="text-text-primary truncate text-sm font-semibold">
              {app.name}
            </span>
            {#if app.oneLiner !== undefined}
              <span class="text-text-tertiary line-clamp-2 text-xs text-pretty">
                {app.oneLiner}
              </span>
            {/if}
          </span>
          <span
            aria-hidden="true"
            class="bg-bg-secondary text-text-primary shrink-0 rounded-full px-3.5 py-1 text-xs font-semibold"
          >
            Open
          </span>
        </a>
      </li>
    {/each}
  </ul>
</section>
