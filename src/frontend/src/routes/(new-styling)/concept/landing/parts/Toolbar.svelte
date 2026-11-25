<!--
  The playground's controls, floating at the top as the concept's pills
  float: the layout (and the ones either side of it), the identities on the
  device, the "Last used" mark, the texture, what the brand side says and
  in which colours, how to look at it (the page, a phone, all at once) and
  the theme. Every choice has a key too; the menus say which.
-->
<script lang="ts">
  import {
    CheckIcon,
    ChevronDownIcon,
    ChevronLeftIcon,
    ChevronRightIcon,
    EyeOffIcon,
    LayoutGridIcon,
    MonitorIcon,
    MoonIcon,
    SmartphoneIcon,
    SunIcon,
  } from "@lucide/svelte";
  import Popup from "../../foundation/frame/Popup.svelte";
  import { FLOATING } from "../../foundation/surfaces/Elevated.svelte";
  import SurfaceLevel from "../../foundation/surfaces/SurfaceLevel.svelte";
  import type { ConceptTheme } from "../../mock/theme.svelte";
  import {
    BRANDS,
    LAYOUTS,
    STATES,
    TEXTURES,
    TONES,
    type Playground,
    type ViewId,
  } from "../playground.svelte";

  interface Props {
    p: Playground;
    theme: ConceptTheme;
    onHide: () => void;
  }

  const { p, theme, onHide }: Props = $props();

  let menu = $state<{
    kind: "layout" | "texture" | "brand";
    anchor: HTMLElement;
  }>();
  const close = () => (menu = undefined);

  const layout = $derived(LAYOUTS.find((one) => one.id === p.layout));
  const texture = $derived(TEXTURES.find((one) => one.id === p.texture));
  const brand = $derived(BRANDS.find((one) => one.id === p.brand));
  const hasBrand = $derived(p.layout === "split" || p.layout === "immersive");

  const VIEWS: {
    id: ViewId;
    label: string;
    icon: typeof MonitorIcon;
    key: string;
  }[] = [
    { id: "page", label: "This window", icon: MonitorIcon, key: "W" },
    { id: "phone", label: "On a phone", icon: SmartphoneIcon, key: "P" },
    { id: "all", label: "All layouts", icon: LayoutGridIcon, key: "O" },
  ];
  const THEMES = [
    { value: "light" as const, label: "Light", icon: SunIcon },
    { value: "dark" as const, label: "Dark", icon: MoonIcon },
  ];

  const PILL =
    "text-text-primary focus-visible:ring-focus-ring flex h-8 shrink-0 items-center gap-1.5 rounded-full px-3 text-[13px] font-medium whitespace-nowrap outline-none transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:ring-2 active:bg-(--ii-selected)";
  const ICON_BUTTON =
    "text-fg-tertiary hover:text-fg-primary focus-visible:ring-focus-ring grid size-8 shrink-0 place-items-center rounded-full outline-none transition-colors duration-150 hover:bg-(--ii-hover) focus-visible:ring-2 active:bg-(--ii-selected)";
  const TRACK = "flex shrink-0 rounded-full bg-(--ii-hover) p-0.5";
  const SEGMENT =
    "focus-visible:ring-focus-ring flex h-7 items-center gap-1.5 rounded-full px-2.5 text-[13px] font-medium whitespace-nowrap outline-none transition-[color,background-color,box-shadow] duration-150 ease-out focus-visible:ring-2 motion-reduce:transition-none";
  const ON = "text-text-primary bg-(--ii-top) shadow-(--ii-shadow-2)";
  const OFF = "text-text-tertiary hover:text-text-primary";
  const DIVIDER = "bg-border-tertiary mx-1 h-5 w-px shrink-0";

  const ROW =
    "btn btn-tertiary h-auto min-h-9 justify-start gap-2.5 !px-3 py-2 text-start";
</script>

<SurfaceLevel level={FLOATING}>
  <div
    role="toolbar"
    aria-label="Landing playground"
    class="ii-elevation-6 flex max-w-[calc(100vw-1rem)] [scrollbar-width:none] items-center gap-0.5 overflow-x-auto rounded-full bg-(--ii-floating) p-1.5"
  >
    <!-- Layout -->
    <button
      type="button"
      class={ICON_BUTTON}
      title="Previous layout (←)"
      aria-label="Previous layout"
      onclick={() => p.stepLayout(-1)}
    >
      <ChevronLeftIcon class="size-4 rtl:-scale-x-100" />
    </button>
    <button
      type="button"
      class={[PILL, "min-w-[8.5rem] justify-between"]}
      aria-haspopup="menu"
      aria-expanded={menu?.kind === "layout"}
      onclick={(event) =>
        (menu = { kind: "layout", anchor: event.currentTarget })}
    >
      <span class="text-text-tertiary tabular-nums">
        {LAYOUTS.findIndex((one) => one.id === p.layout) + 1}
      </span>
      <span class="flex-1 text-start">{layout?.label}</span>
      <ChevronDownIcon class="text-fg-quaternary size-3.5" />
    </button>
    <button
      type="button"
      class={ICON_BUTTON}
      title="Next layout (→)"
      aria-label="Next layout"
      onclick={() => p.stepLayout(1)}
    >
      <ChevronRightIcon class="size-4 rtl:-scale-x-100" />
    </button>

    <span class={DIVIDER} aria-hidden="true"></span>

    <!-- The identities on the device -->
    <div role="radiogroup" aria-label="Identities on the device" class={TRACK}>
      {#each STATES as option, index (option.id)}
        <button
          type="button"
          role="radio"
          aria-checked={p.state === option.id}
          title={`${option.note} (${index + 1})`}
          onclick={() => p.setState(option.id)}
          class={[SEGMENT, p.state === option.id ? ON : OFF]}
        >
          {option.label}
        </button>
      {/each}
    </div>

    <button
      type="button"
      role="switch"
      aria-checked={p.lastUsed}
      title="The mark on the identity used last (L)"
      onclick={() => (p.lastUsed = !p.lastUsed)}
      class={[PILL, !p.lastUsed && "text-text-tertiary"]}
    >
      <span
        aria-hidden="true"
        class={[
          "grid size-4 place-items-center rounded-[5px] transition-colors duration-150",
          p.lastUsed
            ? "bg-fg-primary text-text-primary-inversed"
            : "border-border-primary border-[1.5px]",
        ]}
      >
        {#if p.lastUsed}<CheckIcon class="size-3" strokeWidth={3} />{/if}
      </span>
      Last used
    </button>

    <span class={DIVIDER} aria-hidden="true"></span>

    <button
      type="button"
      class={PILL}
      aria-haspopup="menu"
      aria-expanded={menu?.kind === "texture"}
      title="Texture (T)"
      onclick={(event) =>
        (menu = { kind: "texture", anchor: event.currentTarget })}
    >
      <span class="text-text-tertiary">Texture</span>
      {texture?.label}
      <ChevronDownIcon class="text-fg-quaternary size-3.5" />
    </button>

    {#if hasBrand}
      <button
        type="button"
        class={PILL}
        aria-haspopup="menu"
        aria-expanded={menu?.kind === "brand"}
        title="What the brand side says (B), in which colours (C)"
        onclick={(event) =>
          (menu = { kind: "brand", anchor: event.currentTarget })}
      >
        <span class="text-text-tertiary">Brand</span>
        {brand?.label}
        {#if p.tone === "identity"}
          <span
            aria-hidden="true"
            class="size-2.5 rounded-full outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12"
            style:background-color={p.focusPalette?.background}
          ></span>
        {/if}
        <ChevronDownIcon class="text-fg-quaternary size-3.5" />
      </button>
    {/if}

    <span class={DIVIDER} aria-hidden="true"></span>

    <div role="radiogroup" aria-label="View" class={TRACK}>
      {#each VIEWS as option (option.id)}
        {@const Icon = option.icon}
        <button
          type="button"
          role="radio"
          aria-checked={p.view === option.id}
          title={`${option.label} (${option.key})`}
          onclick={() => (p.view = option.id)}
          class={[SEGMENT, "px-2", p.view === option.id ? ON : OFF]}
        >
          <Icon class="size-3.5" aria-hidden="true" />
          <span class="sr-only">{option.label}</span>
        </button>
      {/each}
    </div>

    <div role="radiogroup" aria-label="Theme" class={[TRACK, "ms-1"]}>
      {#each THEMES as option (option.value)}
        {@const Icon = option.icon}
        <button
          type="button"
          role="radio"
          aria-checked={theme.current === option.value}
          title={`${option.label} (D)`}
          onclick={() => theme.set(option.value)}
          class={[SEGMENT, "px-2", theme.current === option.value ? ON : OFF]}
        >
          <Icon class="size-3.5" aria-hidden="true" />
          <span class="sr-only">{option.label}</span>
        </button>
      {/each}
    </div>

    <button
      type="button"
      class={[ICON_BUTTON, "ms-0.5"]}
      title="Hide the controls (H)"
      aria-label="Hide the controls"
      onclick={onHide}
    >
      <EyeOffIcon class="size-4" />
    </button>
  </div>

  {#if menu !== undefined}
    <Popup
      anchor={menu.anchor}
      onClose={close}
      direction="down"
      align="start"
      distance="0.5rem"
      responsive={false}
      over={FLOATING}
      class="!w-max"
    >
      <div
        class="flex max-h-(--max-panel-height) max-w-[22rem] flex-col overflow-y-auto overscroll-contain p-1.5 *:shrink-0"
        role="menu"
        aria-label={menu.kind === "layout"
          ? "Layout"
          : menu.kind === "texture"
            ? "Texture"
            : "Brand"}
      >
        {#if menu.kind === "layout"}
          {#each ["Priority", "Conventional", "Alternative"] as group (group)}
            <div
              aria-hidden="true"
              class="text-text-tertiary px-3 pt-2 pb-1 text-xs"
            >
              {group}
            </div>
            {#each LAYOUTS.filter((one) => one.group === group) as option (option.id)}
              <button
                type="button"
                role="menuitemradio"
                aria-checked={option.id === p.layout}
                onclick={() => {
                  close();
                  p.setLayout(option.id);
                }}
                class={ROW}
              >
                <span
                  class="text-text-tertiary w-3 shrink-0 text-xs tabular-nums"
                >
                  {LAYOUTS.indexOf(option) + 1}
                </span>
                <span class="flex min-w-0 flex-1 flex-col gap-0.5">
                  <span>{option.label}</span>
                  <span
                    class="text-text-tertiary text-xs font-normal whitespace-normal"
                  >
                    {option.note}
                  </span>
                </span>
                <CheckIcon
                  class={[
                    "text-fg-tertiary size-4 shrink-0",
                    option.id !== p.layout && "invisible",
                  ]}
                  aria-hidden="true"
                />
              </button>
            {/each}
          {/each}
        {:else if menu.kind === "texture"}
          {#each TEXTURES as option (option.id)}
            <button
              type="button"
              role="menuitemradio"
              aria-checked={option.id === p.texture}
              onclick={() => {
                close();
                p.texture = option.id;
              }}
              class={ROW}
            >
              <span class="flex min-w-0 flex-1 flex-col gap-0.5">
                <span>{option.label}</span>
                <span
                  class="text-text-tertiary text-xs font-normal whitespace-normal"
                >
                  {option.note}
                </span>
              </span>
              <CheckIcon
                class={[
                  "text-fg-tertiary size-4 shrink-0",
                  option.id !== p.texture && "invisible",
                ]}
                aria-hidden="true"
              />
            </button>
          {/each}
          <p class="text-text-tertiary px-3 pt-2 pb-1.5 text-[11px] leading-4">
            Magnet to Swell after textures.dev's free set (MIT), redrawn on a
            canvas.
          </p>
        {:else}
          <div
            aria-hidden="true"
            class="text-text-tertiary px-3 pt-2 pb-1 text-xs"
          >
            What it says
          </div>
          {#each BRANDS as option (option.id)}
            <button
              type="button"
              role="menuitemradio"
              aria-checked={option.id === p.brand}
              onclick={() => {
                close();
                p.brand = option.id;
              }}
              class={ROW}
            >
              <span class="flex-1">{option.label}</span>
              <CheckIcon
                class={[
                  "text-fg-tertiary size-4 shrink-0",
                  option.id !== p.brand && "invisible",
                ]}
                aria-hidden="true"
              />
            </button>
          {/each}
          <hr class="border-border-secondary -mx-1.5 my-1.5" />
          <div
            aria-hidden="true"
            class="text-text-tertiary px-3 pt-1.5 pb-1 text-xs"
          >
            In which colours
          </div>
          {#each TONES as option (option.id)}
            <button
              type="button"
              role="menuitemradio"
              aria-checked={option.id === p.tone}
              onclick={() => {
                close();
                p.tone = option.id;
              }}
              class={ROW}
            >
              <span class="flex min-w-0 flex-1 flex-col gap-0.5">
                <span>{option.label}</span>
                <span
                  class="text-text-tertiary text-xs font-normal whitespace-normal"
                >
                  {option.note}
                </span>
              </span>
              <CheckIcon
                class={[
                  "text-fg-tertiary size-4 shrink-0",
                  option.id !== p.tone && "invisible",
                ]}
                aria-hidden="true"
              />
            </button>
          {/each}
        {/if}
      </div>
    </Popup>
  {/if}
</SurfaceLevel>
