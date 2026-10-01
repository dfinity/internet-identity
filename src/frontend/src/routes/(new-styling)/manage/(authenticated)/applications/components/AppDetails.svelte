<script lang="ts">
  import { ArrowUpRightIcon } from "@lucide/svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { getAppMetadataStore } from "$lib/stores/app-metadata.store";
  import { t } from "$lib/stores/locale.store";
  import { originLabel } from "$lib/utils/urlUtils";
  import AppLogo from "./AppLogo.svelte";

  interface Props {
    /** The origin the app's identity is derived for: what its metadata is published
     *  on and its consent is keyed by. */
    origin: string;
    /** Where the user signs in to it from, which is what they know it by. */
    displayOrigin: string;
    /** Whether this Internet Identity notifies for the app at all. Without it there
     *  is nothing to switch, so the switch is left out. */
    canNotify: boolean;
    /** `undefined` while still being read, which holds the switch until it is known. */
    allowed: boolean | undefined;
    saving: boolean;
    onAllowedChange: (allowed: boolean) => void;
  }

  const {
    origin,
    displayOrigin,
    canNotify,
    allowed,
    saving,
    onAllowedChange,
  }: Props = $props();

  // As the sign-in screens ask, so an app whose curated entry names only the origin
  // it is used on still resolves.
  const metadataStore = $derived(getAppMetadataStore(origin, displayOrigin));
  const metadata = $derived($metadataStore);
  const label = $derived(originLabel(displayOrigin));
  const name = $derived(metadata.name ?? label);

  const titleId = $props.id();
  const hintId = `${titleId}-hint`;
</script>

<div class="flex flex-col">
  <!-- Clears the dialog's close button. -->
  <div class="flex flex-row items-center gap-4 pe-8">
    <AppLogo logo={metadata.logo} size="lg" />
    <div class="flex min-w-0 flex-1 flex-col gap-0.5">
      <h2 class="text-text-primary truncate text-xl font-medium">{name}</h2>
      <a
        href={displayOrigin}
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

  {#if canNotify}
    <div aria-hidden="true" class="border-border-tertiary my-6 border-t"></div>
    <div class="flex flex-row items-center gap-4">
      <div class="flex min-w-0 flex-1 flex-col gap-0.5">
        <span id={titleId} class="text-text-primary text-sm font-semibold">
          {$t`Notifications`}
        </span>
        <span id={hintId} class="text-text-tertiary text-sm text-pretty">
          {$t`Let ${name} notify you.`}
        </span>
      </div>
      <div class="flex h-6 shrink-0 items-center">
        <!-- `onclick` rather than `onchange`, so the permission prompt that turning
             it on can raise runs inside the user's gesture. -->
        <Toggle
          checked={allowed === true}
          disabled={allowed === undefined || saving}
          onclick={(event) => onAllowedChange(event.currentTarget.checked)}
          aria-labelledby={titleId}
          aria-describedby={hintId}
        />
      </div>
    </div>
  {/if}

  <a
    href={displayOrigin}
    target="_blank"
    rel="noopener noreferrer"
    class="btn btn-primary btn-xl mt-10 w-full"
  >
    <span>{$t`Open ${name}`}</span>
    <ArrowUpRightIcon class="size-5" />
  </a>
</div>
