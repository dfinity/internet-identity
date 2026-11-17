<script lang="ts">
  import { untrack } from "svelte";
  import { UserIcon } from "@lucide/svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import {
    IDENTITY_PALETTES,
    identityInitial,
  } from "$lib/utils/identityColors";

  interface Props {
    name: string;
    /** Where the colours the identity shows now stand in
     *  `IDENTITY_PALETTES`. */
    paletteIndex: number;
    /** What the identity signs in with, as its card says it. */
    signedInWith: string;
    onSave: (value: { name: string; paletteIndex: number }) => void;
    onClose: () => void;
  }

  const { name, paletteIndex, signedInWith, onSave, onClose }: Props = $props();

  const MAX_NAME_LENGTH = 32;
  /** The swatches stand in rows of seven, three rows for the 21 pairs. */
  const COLUMNS = 7;

  // A draft of both, so nothing changes until Save.
  let draftName = $state(untrack(() => name));
  let draftIndex = $state(untrack(() => paletteIndex));

  const palette = $derived(IDENTITY_PALETTES[draftIndex]);
  const initial = $derived(identityInitial(draftName));
  const canSave = $derived(draftName.trim().length > 0);

  const id = $props.id();
  const titleId = `${id}-title`;
  const nameId = `${id}-name`;
  const nameHintId = `${id}-name-hint`;
  const colourLabelId = `${id}-colour`;

  let swatches = $state<HTMLButtonElement[]>([]);

  // Arrow keys walk the grid and pick as they go, so the preview follows;
  // Tab still reaches every swatch.
  const moveFrom = (event: KeyboardEvent, index: number) => {
    const rtl =
      getComputedStyle(event.currentTarget as Element).direction === "rtl";
    const last = IDENTITY_PALETTES.length - 1;
    const next =
      event.key === "Home"
        ? 0
        : event.key === "End"
          ? last
          : event.key === "ArrowUp"
            ? index - COLUMNS
            : event.key === "ArrowDown"
              ? index + COLUMNS
              : event.key === "ArrowLeft"
                ? index + (rtl ? 1 : -1)
                : event.key === "ArrowRight"
                  ? index + (rtl ? -1 : 1)
                  : undefined;
    if (next === undefined || next < 0 || next > last) {
      return;
    }
    event.preventDefault();
    draftIndex = next;
    swatches[next]?.focus();
  };

  const save = (event: SubmitEvent) => {
    event.preventDefault();
    if (!canSave) {
      return;
    }
    onSave({ name: draftName.trim(), paletteIndex: draftIndex });
  };

  /** The hairline IdentityAvatar draws, so a pale pair stays on a pale page. */
  const HAIRLINE =
    "outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12";
  const PRESS =
    "transition-[scale,box-shadow] duration-160 ease-out active:scale-[0.97] motion-reduce:transition-none motion-reduce:active:scale-100";
</script>

<Dialog {onClose} aria-labelledby={titleId}>
  <form onsubmit={save} class="flex flex-col gap-5">
    <h2
      id={titleId}
      class="text-text-primary pe-10 text-lg leading-6 font-semibold tracking-tight"
    >
      Customize identity
    </h2>

    <!-- What the panel will show, as it is typed and picked. -->
    <div class="flex items-center gap-3 rounded-2xl bg-(--ii-hover) p-3">
      <Avatar
        size="lg"
        aria-hidden="true"
        class={[
          HAIRLINE,
          "transition-colors duration-200 ease-out motion-reduce:transition-none",
        ]}
        style="background-color: {palette.background}; color: {palette.foreground}; border-color: transparent;"
      >
        {#if initial !== undefined}
          {initial}
        {:else}
          <UserIcon class="size-6" />
        {/if}
      </Avatar>
      <div class="flex min-w-0 flex-1 flex-col">
        <span
          class="text-text-primary truncate text-base leading-6 font-semibold"
        >
          {draftName.trim() === "" ? " " : draftName}
        </span>
        <span class="text-text-tertiary truncate text-sm">
          {signedInWith}
        </span>
      </div>
    </div>

    <div class="flex flex-col gap-1.5">
      <div class="flex items-baseline justify-between gap-3">
        <label for={nameId} class="text-text-secondary text-sm font-medium">
          Name
        </label>
        <span class="text-text-tertiary text-sm tabular-nums">
          {draftName.length}/{MAX_NAME_LENGTH}
        </span>
      </div>
      <Input
        id={nameId}
        bind:value={draftName}
        type="text"
        inputmode="text"
        size="sm"
        maxlength={MAX_NAME_LENGTH}
        autocomplete="off"
        autocorrect="off"
        spellcheck="false"
        autofocus
        aria-describedby={nameHintId}
      />
      <p id={nameHintId} class="text-text-tertiary text-sm">
        Shown here and when you pick an identity to sign in.
      </p>
    </div>

    <div class="flex flex-col gap-2.5">
      <div class="flex items-baseline justify-between gap-3">
        <span
          id={colourLabelId}
          class="text-text-secondary text-sm font-medium"
        >
          Colour
        </span>
        <span class="text-text-tertiary truncate text-sm">
          {palette.name}
        </span>
      </div>
      <!-- Each swatch is the avatar it would make: the initial on the pair,
           so what is picked is what the panel shows. -->
      <div
        role="group"
        aria-labelledby={colourLabelId}
        class="grid grid-cols-7 gap-2"
      >
        {#each IDENTITY_PALETTES as pair, index (pair.name)}
          {@const selected = index === draftIndex}
          <button
            bind:this={swatches[index]}
            type="button"
            aria-pressed={selected}
            aria-label={pair.name}
            onclick={() => (draftIndex = index)}
            onkeydown={(event) => moveFrom(event, index)}
            class={[
              "grid aspect-square place-items-center rounded-full text-sm font-semibold",
              "ring-offset-bg-primary_alt ring-offset-2",
              "focus-visible:ring-focus-ring focus-visible:ring-2",
              HAIRLINE,
              PRESS,
              selected && "ring-fg-primary ring-2",
            ]}
            style:background-color={pair.background}
            style:color={pair.foreground}
          >
            {#if initial !== undefined}
              <span aria-hidden="true">{initial}</span>
            {:else}
              <UserIcon class="size-4" aria-hidden="true" />
            {/if}
          </button>
        {/each}
      </div>
    </div>

    <div class="flex gap-3 sm:justify-end">
      <button
        type="button"
        onclick={onClose}
        class="btn btn-secondary font-medium max-sm:flex-1"
      >
        Cancel
      </button>
      <button
        type="submit"
        disabled={!canSave}
        class="btn btn-primary font-medium max-sm:flex-1"
      >
        Save
      </button>
    </div>
  </form>
</Dialog>
