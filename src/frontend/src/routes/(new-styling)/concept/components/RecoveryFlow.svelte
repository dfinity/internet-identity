<script lang="ts" module>
  import {
    emailStatus,
    isOn,
    phraseStatus,
    recoveryOf,
    type MethodStatus,
    type RecoveryState,
  } from "../status";

  export type RecoveryMethod = "phrase" | "email";
  /** Each step takes focus as it arrives, so a screen reader reads where the
   *  user is and a keyboard carries on from the top of the step. The first
   *  one also carries `autofocus`, which the dialog honours as it opens. */
  const arrive = (node: HTMLElement) => {
    node.focus();
  };
</script>

<script lang="ts">
  import {
    BookOpenIcon,
    BriefcaseMedicalIcon,
    CheckIcon,
    ArrowLeftIcon,
    ChevronRightIcon,
    EyeIcon,
    ListChecksIcon,
    MailIcon,
    PowerOffIcon,
    ShieldCheckIcon,
    XIcon,
  } from "@lucide/svelte";
  import { tick } from "svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Sheet from "./Sheet.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { swapIn, viewIn, viewOut } from "../motion";
  import type { Scenario } from "../status";
  import Elevated, { FLOATING, surfaceColor } from "./Elevated.svelte";

  interface Props {
    /** Where recovery stands now; each way back in reads its own status. */
    recovery: RecoveryState;
    identityName: string;
    /** Opens straight on a way back in, as the panel's button for it does. */
    focus?: RecoveryMethod;
    /** A way turned on or off: the scenario moves on, the dialog stays. */
    onChange: (patch: Partial<Scenario>, message: string) => void;
    onClose: () => void;
    /** A phone: a sheet from the bottom, swiped away like the panel's. */
    sheet?: boolean;
  }

  const {
    recovery,
    identityName,
    focus,
    onChange,
    onClose,
    sheet = false,
  }: Props = $props();

  /** `hub` lists both ways back in; the rest belong to the one picked. */
  type Step = "hub" | "save" | "check" | "done" | "manage";

  /** The two steps the progress bar counts; hub, done and manage sit outside. */
  const ORDER: Step[] = ["save", "check"];
  const LABELS: Record<RecoveryMethod, string[]> = {
    phrase: ["Write down", "Check"],
    email: ["Add email", "Confirm"],
  };
  const NAMES: Record<RecoveryMethod, string> = {
    email: "Recovery email",
    phrase: "Recovery phrase",
  };
  const WORDS = [
    "harbor",
    "velvet",
    "maple",
    "orbit",
    "candle",
    "ribbon",
    "lunar",
    "quiet",
    "ember",
    "tunnel",
    "meadow",
    "pixel",
  ];
  /** The words the check asks for, 1-based, as on the user's paper. */
  const CHECKED = [3, 7, 11];
  /** When the concept's identity wrote its phrase down, and last checked it. */
  const WROTE_ON = "14 Nov";

  const uid = $props.id();
  const titleId = `${uid}-title`;
  const codeId = `${uid}-code`;

  let step = $state<Step>("hub");
  let method = $state<RecoveryMethod>();
  /** Checking a phrase that's already on, from manage: nothing is set up. */
  let rechecking = $state(false);
  /** The steps behind this one, so Back returns to where the user came from. */
  let history = $state<Step[]>([]);
  /** +1 going on, −1 going back: the side the next step slides in from. */
  let direction = $state(1);

  let shown = $state(false);
  let wordList = $state<HTMLOListElement>();
  const typed = $state(CHECKED.map(() => ""));
  // A plausible address to start from, as the mock has: "thomas@gmail.com".
  // svelte-ignore state_referenced_locally
  const handle = identityName.toLowerCase().replace(/[^a-z0-9]/g, "");
  let email = $state(`${handle === "" ? "you" : handle}@gmail.com`);
  let code = $state("");
  let resent = $state(false);

  const reduced = $derived(prefersReducedMotion.current);
  const stepIndex = $derived(ORDER.indexOf(step));
  const labels = $derived(method === undefined ? [] : LABELS[method]);
  const statuses = $derived({
    email: emailStatus(recovery),
    phrase: phraseStatus(recovery),
  });
  const onCount = $derived(
    [statuses.email, statuses.phrase].filter(isOn).length,
  );
  const title = $derived(
    step === "hub" || method === undefined ? "Recovery" : NAMES[method],
  );
  // Only where closing loses nothing does a click outside or Escape close it.
  const dismissable = $derived(
    step === "hub" || step === "manage" || step === "done",
  );

  /** The address as the code step shows it back: "th•••@gmail.com". */
  const maskedEmail = $derived.by(() => {
    const at = email.trim().lastIndexOf("@");
    return at < 1
      ? "your inbox"
      : `${email.trim().slice(0, Math.min(2, at))}•••${email.trim().slice(at)}`;
  });

  const readingDirection = () =>
    document.documentElement.dir === "rtl" ? -1 : 1;

  const go = (next: Step) => {
    direction = 1;
    history = [...history, step];
    step = next;
  };

  const back = () => {
    const previous = history.at(-1);
    if (previous === undefined) return;
    direction = -1;
    history = history.slice(0, -1);
    if (previous === "manage" || previous === "hub") {
      rechecking = false;
    }
    step = previous;
  };

  /** Back to both ways, from wherever the user is. */
  const home = () => {
    direction = -1;
    history = [];
    rechecking = false;
    shown = false;
    step = "hub";
  };

  /** A row in the hub: set the way up, finish it, or manage it. */
  const open = (picked: RecoveryMethod) => {
    method = picked;
    const status = statuses[picked];
    if (status === "off") {
      go("save");
    } else if (status === "unchecked" || status === "pending") {
      // Written down but never checked, or sent and never confirmed: only
      // the last step is left.
      go("check");
    } else {
      go("manage");
    }
  };

  // Opened from the panel's button for one way: straight to it.
  // svelte-ignore state_referenced_locally
  if (focus !== undefined) open(focus);

  /** Where recovery stands once one way is set to `status`, the other kept. */
  const withWay = (
    which: RecoveryMethod,
    status: MethodStatus,
  ): RecoveryState =>
    which === "phrase"
      ? recoveryOf(status, statuses.email)
      : recoveryOf(statuses.phrase, status);

  /** Recovery is on with this way now: the scenario moves on at once, so the
   *  row in the hub already says so when the user goes back to it. */
  const finish = () => {
    if (
      method === "phrase" &&
      (!rechecking || statuses.phrase === "unchecked")
    ) {
      onChange({ recovery: withWay("phrase", "on") }, "Recovery phrase is on");
    } else if (method === "email") {
      onChange({ recovery: withWay("email", "on") }, "Recovery email is on");
    }
    go("done");
  };

  const turnOff = (which: RecoveryMethod) => {
    onChange(
      { recovery: withWay(which, "off") },
      which === "phrase"
        ? "Recovery phrase turned off"
        : "Recovery email turned off",
    );
    home();
  };

  const next = () => {
    if (step === "save") go("check");
    else if (step === "check") finish();
  };

  const reveal = async () => {
    shown = true;
    // The button goes with the blur; keep focus at the words it uncovered.
    await tick();
    wordList?.focus();
  };

  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
  const PRIMARY = ["btn btn-primary rounded-full font-medium", PRESS];
  const TERTIARY = "btn btn-tertiary rounded-full font-medium";

  const ROWS = [
    {
      value: "email" as const,
      icon: MailIcon,
      title: "Recovery email",
      off: "A code to your inbox gets you back in",
    },
    {
      value: "phrase" as const,
      icon: BookOpenIcon,
      title: "Recovery phrase",
      off: "12 words you write down and keep offline",
    },
  ];

  /** What a row says under its name, and how loudly. */
  const said = (which: RecoveryMethod): { meta: string; badge?: string } => {
    const status = statuses[which];
    const row = ROWS.find(({ value }) => value === which);
    if (status === "off") return { meta: row?.off ?? "" };
    if (status === "unchecked")
      return { meta: "Written down, not checked yet" };
    if (status === "pending") return { meta: `Code sent to t•••@gmail.com` };
    if (status === "inUse")
      return { meta: "You're signed in with it now", badge: "In use" };
    if (status === "new") return { meta: "Reset 2 hours ago", badge: "On" };
    return which === "email"
      ? { meta: "t•••@gmail.com", badge: "On" }
      : { meta: `Checked on ${WROTE_ON}`, badge: "On" };
  };
</script>

<!-- Where the user is, what they can do now, and why: on every step, so the
     flow explains itself instead of leaving that to a banner elsewhere. -->
{#snippet context(Icon: typeof MailIcon, heading: string, body: string)}
  <!-- svelte-ignore a11y_autofocus -->
  <div
    tabindex="-1"
    autofocus
    use:arrive
    class="flex gap-3 rounded-2xl bg-(--ii-hover) p-3.5 outline-none"
  >
    <!-- A step above what holds the flow: a sheet on a phone, which floats;
         on desktop the product's dialog, which is darker in dark. -->
    <span
      aria-hidden="true"
      class="text-fg-secondary grid size-9 shrink-0 place-items-center rounded-[0.625rem] shadow-(--ii-shadow-2)"
      style:background-color={surfaceColor(sheet ? FLOATING + 1 : FLOATING)}
    >
      <Icon class="size-4.5" />
    </span>
    <div class="flex min-w-0 flex-col gap-0.5">
      <p class="text-text-primary text-sm font-semibold text-pretty">
        {heading}
      </p>
      <p class="text-text-secondary text-[13px] leading-[18px] text-pretty">
        {body}
      </p>
    </div>
  </div>
{/snippet}

{#snippet manageRow(
  Icon: typeof MailIcon,
  heading: string,
  meta: string | undefined,
  danger: boolean,
  onclick: () => void,
)}
  <li class="flex">
    <button
      type="button"
      {onclick}
      class="ii-hover focus-visible:ring-focus-ring flex w-full items-center gap-3 px-4 py-3 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset"
    >
      <span
        aria-hidden="true"
        class={[
          "grid size-9 shrink-0 place-items-center rounded-[0.625rem]",
          danger
            ? "text-fg-error-primary bg-(--ii-error-tint)"
            : "text-fg-secondary bg-(--ii-selected)",
        ]}
      >
        <Icon class="size-4.5" />
      </span>
      <span class="flex min-w-0 flex-1 flex-col gap-0.5">
        <span
          class={[
            "text-sm font-medium",
            danger ? "text-text-error-primary" : "text-text-primary",
          ]}
        >
          {heading}
        </span>
        {#if meta !== undefined}
          <span class="text-text-tertiary text-[13px] leading-[18px]">
            {meta}
          </span>
        {/if}
      </span>
      {#if !danger}
        <ChevronRightIcon
          aria-hidden="true"
          class="text-fg-quaternary size-4 shrink-0 rtl:-scale-x-100"
        />
      {/if}
    </button>
  </li>
{/snippet}

{#if sheet}
  <Sheet {onClose} {dismissable} aria-labelledby={titleId}>
    {@render body()}
  </Sheet>
{:else}
  <Dialog
    {onClose}
    showCloseButton={false}
    closeOnOutsideClick={dismissable}
    aria-labelledby={titleId}
  >
    {@render body()}
  </Dialog>
{/if}

{#snippet body()}
  <header class="mb-4 flex items-center gap-2">
    <!-- Inside one way, the way back to both stays at hand. -->
    {#if step !== "hub"}
      <button
        type="button"
        onclick={home}
        class="btn btn-tertiary btn-icon btn-sm -ms-2 shrink-0 rounded-full"
      >
        <ArrowLeftIcon class="size-5 rtl:-scale-x-100" aria-hidden="true" />
        <span>All of recovery</span>
      </button>
    {/if}
    <h2
      id={titleId}
      class={[
        "text-text-primary min-w-0 flex-1 truncate text-base font-semibold",
        step === "done" && "sr-only",
      ]}
    >
      {title}
    </h2>
    <button
      type="button"
      onclick={onClose}
      class="btn btn-tertiary btn-icon btn-sm ms-auto -me-2 shrink-0 rounded-full"
    >
      <XIcon class="size-5" aria-hidden="true" />
      <span>Close</span>
    </button>
  </header>

  {#if stepIndex >= 0 && !rechecking}
    <ol aria-label="Steps" class="mb-4 grid grid-cols-2 gap-1.5">
      {#each labels as label, index (index)}
        <li
          aria-current={index === stepIndex ? "step" : undefined}
          class="flex min-w-0 flex-col gap-1.5"
        >
          <span
            aria-hidden="true"
            class={[
              "h-1 rounded-full transition-colors duration-300 ease-(--ii-ease-out) motion-reduce:transition-none",
              index <= stepIndex ? "bg-fg-primary" : "bg-(--ii-selected)",
            ]}
          ></span>
          <span
            class={[
              "truncate text-xs",
              index === stepIndex
                ? "text-text-primary font-semibold"
                : "text-text-tertiary",
            ]}
          >
            {index + 1} · {label}
          </span>
        </li>
      {/each}
    </ol>
  {/if}

  <!-- Steps stack in one cell, so the old one fades while the new arrives. -->
  <div class="grid">
    {#key step}
      <form
        novalidate
        onsubmit={(event) => {
          event.preventDefault();
          next();
        }}
        class="col-start-1 row-start-1 flex min-w-0 flex-col gap-4"
        in:viewIn={{ direction: direction * readingDirection(), reduced }}
        out:viewOut={{ reduced }}
      >
        {#if step === "hub"}
          <!-- Both ways back in, always in view, each with where it stands:
               rows like the panel's, so recovery reads as one thing with
               two parts, not two separate settings. -->
          {#if onCount === 2}
            {@render context(
              ShieldCheckIcon,
              "You're covered two ways",
              `If you lose your passkeys, either one gets you back in to ${identityName}.`,
            )}
          {:else if onCount === 1}
            <!-- One way is enough: the other stays a choice, not a to-do. -->
            {@render context(
              ShieldCheckIcon,
              "You're covered",
              `If you lose your passkeys, your ${isOn(statuses.phrase) ? "recovery phrase" : "recovery email"} gets you back in to ${identityName}.`,
            )}
          {:else if statuses.phrase === "unchecked"}
            <!-- Written down, never checked: it may not work when needed. -->
            {@render context(
              ListChecksIcon,
              "Check your recovery phrase",
              `It's written down but hasn't been checked. A word copied wrong only shows when you need it to get back in to ${identityName}.`,
            )}
          {:else if statuses.email === "pending"}
            {@render context(
              MailIcon,
              "Confirm your recovery email",
              "Enter the code we sent to turn it on. Until then it can't get you back in.",
            )}
          {:else}
            {@render context(
              BriefcaseMedicalIcon,
              `Right now only your passkeys open ${identityName}`,
              "If you lose them, recovery is your way back in. Set up one way or both. Each takes about two minutes.",
            )}
          {/if}
          <Elevated
            offset={1}
            as="ul"
            class="ii-rows flex flex-col overflow-hidden rounded-2xl [--ii-rows-inset:4rem]"
          >
            {#each ROWS as row (row.value)}
              {@const Icon = row.icon}
              {@const status = statuses[row.value]}
              {@const line = said(row.value)}
              <li class="flex">
                <button
                  type="button"
                  onclick={() => open(row.value)}
                  class="ii-hover focus-visible:ring-focus-ring flex w-full items-center gap-3 px-4 py-3.5 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset"
                >
                  <span
                    aria-hidden="true"
                    class={[
                      "grid size-9 shrink-0 place-items-center rounded-[0.625rem]",
                      status === "off"
                        ? "border-border-secondary text-fg-quaternary border border-dashed"
                        : "text-fg-secondary bg-(--ii-selected)",
                    ]}
                  >
                    <Icon class="size-4.5" />
                  </span>
                  <span class="flex min-w-0 flex-1 flex-col gap-0.5">
                    <span class="flex flex-wrap items-center gap-x-2 gap-y-0.5">
                      <span class="text-text-primary text-sm font-medium">
                        {row.title}
                      </span>
                      {#if line.badge !== undefined}
                        <Badge size="sm" color="success" dot>{line.badge}</Badge
                        >
                      {/if}
                    </span>
                    <span
                      class="text-text-tertiary text-[13px] leading-[18px] text-pretty"
                    >
                      {line.meta}
                    </span>
                  </span>
                  {#if status === "off" || status === "unchecked" || status === "pending"}
                    <span
                      aria-hidden="true"
                      class={[
                        "btn btn-sm shrink-0 rounded-full font-medium",
                        onCount === 0 &&
                        (status !== "off" ||
                          (statuses.phrase !== "unchecked" &&
                            statuses.email !== "pending"))
                          ? "btn-primary"
                          : "btn-secondary",
                      ]}
                    >
                      {status === "off"
                        ? "Set up"
                        : status === "unchecked"
                          ? "Verify"
                          : "Enter code"}
                    </span>
                  {:else}
                    <ChevronRightIcon
                      aria-hidden="true"
                      class="text-fg-quaternary size-4 shrink-0 rtl:-scale-x-100"
                    />
                  {/if}
                </button>
              </li>
            {/each}
          </Elevated>
          <p
            class="text-text-tertiary px-1 text-[13px] leading-[18px] text-pretty"
          >
            To get back in, go to id.ai and choose “Lost access”.
          </p>
        {:else if step === "save" && method === "phrase"}
          {@render context(
            BookOpenIcon,
            "Write these 12 words down",
            `On paper, in this order. Anyone who has them can open ${identityName}, so keep them offline. We will never ask you for them.`,
          )}
          <div class="relative">
            <ol
              bind:this={wordList}
              dir="ltr"
              tabindex="-1"
              aria-label="Your recovery phrase"
              aria-hidden={!shown}
              class={[
                "grid grid-cols-3 gap-1.5 outline-none",
                "transition-[filter] duration-200 ease-out motion-reduce:transition-none",
                !shown && "blur-sm select-none",
              ]}
            >
              {#each WORDS as word, index (index)}
                <li
                  class="border-border-secondary flex min-w-0 items-baseline gap-1.5 rounded-lg border px-2 py-1.5"
                >
                  <span
                    class="text-text-tertiary w-4 shrink-0 text-xs tabular-nums"
                  >
                    {index + 1}
                  </span>
                  <span class="text-text-primary truncate text-sm">{word}</span>
                </li>
              {/each}
            </ol>
            {#if !shown}
              <div class="absolute inset-0 grid place-items-center">
                <button
                  type="button"
                  onclick={reveal}
                  class="btn btn-secondary btn-sm rounded-full font-medium"
                >
                  <EyeIcon class="size-4" aria-hidden="true" />
                  Show words
                </button>
              </div>
            {/if}
          </div>
          <div class="flex items-center justify-between gap-2">
            <button type="button" onclick={back} class={TERTIARY}>Back</button>
            <button type="submit" disabled={!shown} class={PRIMARY}>
              I wrote them down
            </button>
          </div>
        {:else if step === "save"}
          {@render context(
            MailIcon,
            "Which inbox should we use?",
            "When you need to get back in, we send a code here. Pick an address you'll keep for years.",
          )}
          <Input
            label="Email"
            type="email"
            autocomplete="email"
            autocapitalize="off"
            spellcheck={false}
            bind:value={email}
          />
          <div class="flex items-center justify-between gap-2">
            <button type="button" onclick={back} class={TERTIARY}>Back</button>
            <button type="submit" class={PRIMARY}>Send code</button>
          </div>
        {:else if step === "check" && method === "phrase"}
          {#if rechecking}
            {@render context(
              ListChecksIcon,
              "Is your paper still right?",
              "Type three words from it, not from memory. If they match, you can still get back in with it.",
            )}
          {:else}
            {@render context(
              ListChecksIcon,
              "Last step: check three words",
              "So a typo today doesn't lock you out later. Look at your paper, not this screen.",
            )}
          {/if}
          <div class="flex flex-col gap-2">
            {#each CHECKED as number, index (number)}
              <div class="flex items-center gap-3">
                <label
                  for={`${uid}-word-${number}`}
                  class="text-text-tertiary w-16 shrink-0 text-sm"
                >
                  Word {number}
                </label>
                <!-- A fresh setup shows the word as a hint, so trying the
                     concept doesn't mean writing it down. -->
                <Input
                  id={`${uid}-word-${number}`}
                  class="min-w-0 flex-1"
                  autocomplete="off"
                  autocapitalize="off"
                  spellcheck={false}
                  placeholder={rechecking ? undefined : WORDS[number - 1]}
                  bind:value={typed[index]}
                />
              </div>
            {/each}
          </div>
          <div class="flex items-center justify-between gap-2">
            <button type="button" onclick={back} class={TERTIARY}>
              Back
            </button>
            <button type="submit" class={PRIMARY}>Finish</button>
          </div>
        {:else if step === "check"}
          {@render context(
            MailIcon,
            "Check your inbox",
            `We sent a 6-digit code to ${maskedEmail}. It works for 10 minutes.`,
          )}
          <div class="flex flex-col gap-1.5">
            <div class="flex items-center justify-between gap-2">
              <label
                for={codeId}
                class="text-text-secondary text-sm font-medium"
              >
                Code
              </label>
              <button
                type="button"
                disabled={resent}
                onclick={() => (resent = true)}
                class="text-text-secondary disabled:text-text-tertiary rounded-sm text-sm font-medium not-disabled:hover:underline focus-visible:underline focus-visible:outline-none"
              >
                {resent ? "Sent again" : "Resend"}
              </button>
            </div>
            <Input
              id={codeId}
              inputmode="numeric"
              autocomplete="one-time-code"
              placeholder="000 000"
              bind:value={code}
            />
          </div>
          <div class="flex items-center justify-between gap-2">
            <button type="button" onclick={back} class={TERTIARY}>Back</button>
            <button type="submit" class={PRIMARY}>Confirm</button>
          </div>
        {:else if step === "done"}
          <div
            tabindex="-1"
            use:arrive
            class="flex flex-col items-center gap-2 pt-2 text-center outline-none"
          >
            <span
              aria-hidden="true"
              class="bg-bg-success-primary text-fg-success-primary mb-1 grid size-12 place-items-center rounded-full"
            >
              <span class="grid" in:swapIn|global={{ reduced, delay: 80 }}>
                <CheckIcon class="size-6" strokeWidth={2.5} />
              </span>
            </span>
            <h3 class="text-text-primary text-lg font-semibold">
              {rechecking
                ? "Your phrase still works"
                : method === "email"
                  ? "Recovery email is on"
                  : "Recovery phrase is on"}
            </h3>
            <p class="text-text-tertiary max-w-[32ch] text-sm text-pretty">
              {#if method === "email" && !rechecking}
                If you ever lose your passkeys, go to id.ai, choose “Lost
                access” and we'll email you a code.
              {:else}
                If you ever lose your passkeys, go to id.ai, choose “Lost
                access” and type your 12 words.
              {/if}
            </p>
          </div>
          <div class="mt-2 flex justify-center">
            <button
              type="button"
              onclick={home}
              class={[...PRIMARY, "min-w-32"]}
            >
              Done
            </button>
          </div>
        {:else if step === "manage" && method === "phrase"}
          {@render context(
            ShieldCheckIcon,
            "Your phrase is on",
            `Your 12 words get you back in to ${identityName}. You last checked them on ${WROTE_ON}.`,
          )}
          <ul
            class="ii-rows border-border-tertiary flex flex-col overflow-hidden rounded-2xl border [--ii-rows-inset:4rem]"
          >
            {@render manageRow(
              ListChecksIcon,
              "Check my phrase",
              "Make sure your paper still matches",
              false,
              () => {
                rechecking = true;
                go("check");
              },
            )}
            {@render manageRow(
              PowerOffIcon,
              "Turn off the phrase",
              undefined,
              true,
              () => turnOff("phrase"),
            )}
          </ul>
        {:else if step === "manage"}
          {@render context(
            ShieldCheckIcon,
            "Your recovery email is on",
            "If you lose your passkeys, we send a code to t•••@gmail.com.",
          )}
          <ul
            class="ii-rows border-border-tertiary flex flex-col overflow-hidden rounded-2xl border [--ii-rows-inset:4rem]"
          >
            {@render manageRow(
              MailIcon,
              "Change the email",
              "Use another inbox",
              false,
              () => go("save"),
            )}
            {@render manageRow(
              PowerOffIcon,
              "Turn off the email",
              undefined,
              true,
              () => turnOff("email"),
            )}
          </ul>
        {/if}
      </form>
    {/key}
  </div>
{/snippet}
