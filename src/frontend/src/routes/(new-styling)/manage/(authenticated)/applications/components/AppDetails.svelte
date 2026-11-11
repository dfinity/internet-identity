<script lang="ts">
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import { t } from "$lib/stores/locale.store";
  import { originLabel } from "$lib/utils/urlUtils";
  import AppLogo from "./AppLogo.svelte";
  import { appUrl } from "../apps";

  interface Props {
    /** For the app's name, which is what names the dialog. */
    titleId: string;
    /** The origin the app's identity is derived for: what its metadata is published
     *  on and its consent is keyed by. */
    origin: string;
    /** Whether this Internet Identity notifies at all. Without it there is nothing to
     *  switch, so the switch is left out. */
    canNotify: boolean;
    /** Whether the app may notify. */
    allowed: boolean;
    /** Whether a permission change is in flight, which holds every other change back. */
    saving: boolean;
    onToggle: (allowed: boolean) => void;
    onForget: () => void;
  }

  const {
    titleId,
    origin,
    canNotify,
    allowed,
    saving,
    onToggle,
    onForget,
  }: Props = $props();

  const metadataStore = $derived(getAppMetadataStore(origin));
  const metadata = $derived($metadataStore);
  const url = $derived(appUrl(origin));
  const label = $derived(originLabel(url));
  const name = $derived(metadata.name ?? label);

  const switchId = $props.id();
  const hintId = `${switchId}-hint`;
</script>

<div class="flex flex-col">
  <!-- Clears the dialog's close button. -->
  <div class="flex flex-row items-center gap-4 pe-8">
    <AppLogo logo={metadata.logo} size="lg" />
    <div class="flex min-w-0 flex-1 flex-col gap-0.5">
      <h2 id={titleId} class="text-text-primary truncate text-xl font-medium">
        {name}
      </h2>
      <a
        href={url}
        target="_blank"
        rel="noopener noreferrer"
        class="text-text-tertiary self-start text-sm hover:underline focus-visible:underline"
      >
        {label}
      </a>
    </div>
  </div>

  {#if metadata.description !== undefined}
    <p class="text-text-tertiary mt-5 text-base text-pretty">
      {metadata.description}
    </p>
  {/if}

  <div aria-hidden="true" class="border-border-tertiary my-6 border-t"></div>
  <div class="flex flex-col gap-6">
    {#if canNotify}
      <div class="flex flex-row items-center gap-4">
        <div class="flex min-w-0 flex-1 flex-col gap-0.5">
          <span id={switchId} class="text-text-primary text-sm font-semibold">
            {$t`Notifications`}
          </span>
          <span id={hintId} class="text-text-tertiary text-sm text-pretty">
            {$t`Let ${name} notify you.`}
          </span>
        </div>
        <div class="flex h-6 shrink-0 items-center">
          <Toggle
            checked={allowed}
            disabled={saving}
            onchange={(event) => onToggle(event.currentTarget.checked)}
            aria-labelledby={switchId}
            aria-describedby={hintId}
          />
        </div>
      </div>
    {/if}
    <div class="flex flex-row flex-wrap items-center gap-x-4 gap-y-3">
      <div class="flex min-w-50 flex-1 flex-col gap-0.5">
        <span class="text-text-primary text-sm font-semibold">
          {$t`Remove this app`}
        </span>
        <span class="text-text-tertiary text-sm text-pretty">
          {$t`Signs you out and clears its settings.`}
        </span>
      </div>
      <button
        onclick={onForget}
        disabled={saving}
        class="btn btn-secondary btn-sm btn-danger shrink-0"
      >
        {$t`Remove`}
      </button>
    </div>
  </div>
</div>
