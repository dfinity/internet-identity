<script lang="ts" module>
  export type RecoveryMethod = "phrase" | "email";
  export type RecoveryFlowMode = "start" | "resume" | "manage";

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
  import Input from "$lib/components/ui/Input.svelte";
  import { swapIn, viewIn, viewOut } from "../motion";

  interface Props {
    /** `start` opens on the choice, `resume` on the phrase check a user left
     *  unfinished, `manage` on what to do with recovery that's already on. */
    mode: RecoveryFlowMode;
    identityName: string;
    /** Done pressed on the success step: recovery is on with this method. */
    onDone: (method: RecoveryMethod) => void;
    onClose: () => void;
    /** "Turn off recovery" while managing; closes the flow when not given. */
    onTurnOff?: () => void;
  }

  const { mode, identityName, onDone, onClose, onTurnOff }: Props = $props();

  type Step = "choose" | "save" | "check" | "done" | "manage";

  /** The three steps the progress bar counts; done and manage sit outside. */
  const ORDER: Step[] = ["choose", "save", "check"];
  const LABELS: Record<RecoveryMethod | "none", string[]> = {
    phrase: ["Choose", "Write down", "Check"],
    email: ["Choose", "Add email", "Confirm"],
    none: ["Choose", "Set up", "Check"],
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

  // The entry point is fixed for the flow's life, like the button that
  // opened it; the parent mounts a new flow for another one.
  // svelte-ignore state_referenced_locally
  let step = $state<Step>(
    mode === "manage" ? "manage" : mode === "resume" ? "check" : "choose",
  );
  // svelte-ignore state_referenced_locally
  let method = $state<RecoveryMethod | undefined>(
    mode === "start" ? undefined : "phrase",
  );
  /** Came back to check a phrase written down earlier but never checked. */
  // svelte-ignore state_referenced_locally
  let resuming = $state(mode === "resume");
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
  const labels = $derived(LABELS[method ?? "none"]);
  const title = $derived(mode === "manage" ? "Recovery" : "Turn on recovery");
  // Only where closing loses nothing does a click outside or Escape close it.
  const dismissable = $derived(step === "choose" || step === "manage");

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
    if (previous === "manage") {
      rechecking = false;
      method = "phrase";
    }
    step = previous;
  };

  const next = () => {
    if (step === "choose" && method !== undefined) go("save");
    else if (step === "save") go("check");
    else if (step === "check") go("done");
  };

  const startOver = () => {
    direction = -1;
    history = [];
    resuming = false;
    method = undefined;
    shown = false;
    step = "choose";
  };

  const reveal = async () => {
    shown = true;
    // The button goes with the blur; keep focus at the words it uncovered.
    await tick();
    wordList?.focus();
  };

  // On the success step recovery is already on, so closing it counts as Done.
  const close = () => {
    if (step === "done" && method !== undefined) {
      onDone(method);
    } else {
      onClose();
    }
  };

  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
  const PRIMARY = ["btn btn-primary rounded-full font-medium", PRESS];
  const TERTIARY = "btn btn-tertiary rounded-full font-medium";

  const OPTIONS = [
    {
      value: "email" as const,
      icon: MailIcon,
      title: "Email",
      meta: "Good if you'd rather not keep anything on paper. We send a sign-in link to your inbox.",
    },
    {
      value: "phrase" as const,
      icon: BookOpenIcon,
      title: "Recovery phrase",
      meta: "Good if you want it to work without email. 12 words you write down and keep offline.",
    },
  ];
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
    <span
      aria-hidden="true"
      class="text-fg-secondary grid size-9 shrink-0 place-items-center rounded-[0.625rem] bg-(--ii-surface-6) shadow-(--ii-shadow-2)"
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
            ? "bg-bg-error-secondary text-fg-error-primary"
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

<Dialog
  onClose={close}
  showCloseButton={false}
  closeOnOutsideClick={dismissable}
  aria-labelledby={titleId}
>
  <header class="mb-4 flex items-center gap-3">
    <!-- On the success step the result leads, so the title steps aside. -->
    <h2
      id={titleId}
      class={[
        "text-text-primary min-w-0 flex-1 text-base font-semibold",
        step === "done" && "sr-only",
      ]}
    >
      {title}
    </h2>
    <button
      type="button"
      onclick={close}
      class="btn btn-tertiary btn-icon btn-sm ms-auto -me-2 shrink-0 rounded-full"
    >
      <XIcon class="size-5" aria-hidden="true" />
      <span>Close</span>
    </button>
  </header>

  {#if stepIndex >= 0 && !rechecking}
    <ol aria-label="Steps" class="mb-4 grid grid-cols-3 gap-1.5">
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
        {#if step === "choose"}
          {@render context(
            BriefcaseMedicalIcon,
            `Right now only your passkeys open ${identityName}`,
            "If you lose them, recovery is your way back in. Pick one way. It takes about two minutes, and you can change it later.",
          )}
          <fieldset class="flex min-w-0 flex-col gap-2">
            <legend class="sr-only">How to get back in</legend>
            {#each OPTIONS as option (option.value)}
              {@const Icon = option.icon}
              <label
                class={[
                  "ii-hover flex cursor-pointer items-start gap-3 rounded-2xl border p-3.5 transition-[border-color,box-shadow] duration-150 ease-out motion-reduce:transition-none",
                  "has-[:focus-visible]:outline-focus-ring has-[:focus-visible]:outline-2 has-[:focus-visible]:outline-offset-2",
                  method === option.value
                    ? "border-fg-primary ring-fg-primary ring-1"
                    : "border-border-secondary",
                ]}
              >
                <input
                  type="radio"
                  name={`${uid}-method`}
                  value={option.value}
                  bind:group={method}
                  class="sr-only"
                />
                <span
                  aria-hidden="true"
                  class="text-fg-secondary grid size-9 shrink-0 place-items-center rounded-[0.625rem] bg-(--ii-selected)"
                >
                  <Icon class="size-4.5" />
                </span>
                <span class="flex min-w-0 flex-1 flex-col gap-0.5">
                  <span class="text-text-primary text-sm font-medium">
                    {option.title}
                  </span>
                  <span
                    class="text-text-tertiary text-[13px] leading-[18px] text-pretty"
                  >
                    {option.meta}
                  </span>
                </span>
              </label>
            {/each}
          </fieldset>
          <div class="flex items-center justify-between gap-2">
            <button type="button" onclick={onClose} class={TERTIARY}>
              Not now
            </button>
            <button
              type="submit"
              disabled={method === undefined}
              class={PRIMARY}
            >
              Continue
            </button>
          </div>
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
            "When you need to get back in, we send a link here. Pick an address you'll keep for years.",
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
          {:else if resuming}
            {@render context(
              ListChecksIcon,
              "You're one step from done",
              `You wrote down a phrase on ${WROTE_ON} but didn't check it. Type three of its words and recovery is on.`,
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
                  placeholder={resuming || rechecking
                    ? undefined
                    : WORDS[number - 1]}
                  bind:value={typed[index]}
                />
              </div>
            {/each}
          </div>
          <div class="flex items-center justify-between gap-2">
            {#if resuming}
              <button type="button" onclick={startOver} class={TERTIARY}>
                Lost them? Start over
              </button>
            {:else}
              <button type="button" onclick={back} class={TERTIARY}>
                Back
              </button>
            {/if}
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
              {rechecking ? "Your phrase still works" : "Recovery is on"}
            </h3>
            <p class="text-text-tertiary max-w-[32ch] text-sm text-pretty">
              {#if rechecking}
                Keep that paper offline. If you ever lose your passkeys, go to
                id.ai, choose “Lost access” and type your 12 words.
              {:else if method === "phrase"}
                If you ever lose your passkeys, go to id.ai, choose “Lost
                access” and type your 12 words.
              {:else}
                If you ever lose your passkeys, go to id.ai, choose “Lost
                access” and we'll email you a link.
              {/if}
            </p>
          </div>
          <div class="mt-2 flex justify-center">
            <button
              type="button"
              onclick={() => method !== undefined && onDone(method)}
              class={[...PRIMARY, "min-w-32"]}
            >
              Done
            </button>
          </div>
        {:else if step === "manage"}
          {@render context(
            ShieldCheckIcon,
            "You're covered",
            `${identityName} can be recovered with your recovery phrase. You last checked it on ${WROTE_ON}.`,
          )}
          <ul
            class="ii-rows border-border-secondary flex flex-col overflow-hidden rounded-2xl border [--ii-rows-inset:4rem]"
          >
            {@render manageRow(
              ListChecksIcon,
              "Check my phrase",
              "Make sure your paper still matches",
              false,
              () => {
                rechecking = true;
                method = "phrase";
                go("check");
              },
            )}
            {@render manageRow(
              MailIcon,
              "Switch to email",
              "Use a sign-in link instead of words",
              false,
              () => {
                method = "email";
                go("save");
              },
            )}
            {@render manageRow(
              PowerOffIcon,
              "Turn off recovery",
              undefined,
              true,
              onTurnOff ?? onClose,
            )}
          </ul>
          <div class="flex justify-end">
            <button
              type="button"
              onclick={onClose}
              class="btn btn-secondary rounded-full font-medium"
            >
              Close
            </button>
          </div>
        {/if}
      </form>
    {/key}
  </div>
</Dialog>
