<script lang="ts">
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** dApp name for the copy, or undefined when it isn't known. */
    appName: string | undefined;
    /** The app the notifications would come from. Its hostname stands in for the
     * name where the app has not published one, which is what a delivered
     * notification does too. */
    origin: string;
    /** True while the request is in flight. */
    busy: boolean;
    onEnable: () => void;
    onSkip: () => void;
  }

  const { appName, origin, busy, onEnable, onSkip }: Props = $props();

  const app = $derived(appName ?? $t`this app`);
  /** What a delivered notification leads its title with: the published name, or the
   *  hostname where there is none. See `senderOf` in the worker's wake-up. */
  const sender = $derived(appName ?? new URL(origin).hostname);

  const samples = $derived([
    {
      title: $t`Reminder`,
      body: $t`Your event starts in 15 minutes.`,
      at: $t`5m`,
    },
    {
      title: $t`Request approved`,
      body: $t`Your request was approved.`,
      at: $t`2m`,
    },
    { title: $t`New message`, body: $t`You have 1 new message.`, at: $t`now` },
  ]);
</script>

<div
  class="flex flex-1 flex-col items-stretch p-4 sm:max-w-100 sm:justify-center sm:self-center"
>
  <!-- Notification previews. Hidden from assistive technology: the messages in them
       are made up, and a screen reader would read them ahead of the heading as
       though they were real. -->
  <div class="flex flex-col gap-2.5 pt-2 pb-6" aria-hidden="true">
    {#each samples as sample, index (sample.title)}
      <div
        class={[
          "border-border-tertiary flex items-start gap-3 rounded-2xl border bg-white/3 p-3",
          index === samples.length - 1
            ? "border-border-secondary bg-white/8 shadow-lg backdrop-blur-sm"
            : "opacity-90",
        ]}
      >
        <span
          class="border-border-tertiary bg-bg-tertiary size-8 shrink-0 rounded-lg border"
        ></span>
        <div class="min-w-0 flex-1">
          <div class="flex items-baseline justify-between gap-2">
            <span class="text-text-primary truncate text-[13px] font-semibold">
              {sender} · {sample.title}
            </span>
            <span class="text-text-tertiary shrink-0 text-[11px]"
              >{sample.at}</span
            >
          </div>
          <div class="text-text-secondary text-[13px]">{sample.body}</div>
        </div>
      </div>
    {/each}
  </div>

  <h1 class="text-text-primary text-2xl font-medium text-balance">
    {$t`Let ${app} notify you`}
  </h1>
  <p class="text-text-secondary mt-2 text-sm">
    {$t`Get notifications from ${app} when something needs your attention. You can turn them off anytime.`}
  </p>

  <div class="mt-7 flex flex-col gap-2.5">
    <button class="btn btn-primary" onclick={onEnable} disabled={busy}>
      {busy ? $t`Setting up…` : $t`Allow`}
    </button>
    <button class="btn btn-tertiary" onclick={onSkip} disabled={busy}>
      {$t`Not now`}
    </button>
  </div>
</div>
