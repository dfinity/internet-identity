<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";

  const words = $derived<string[]>([
    $t`Face ID`,
    $t`a security key`,
    $t`your phone`,
    $t`a pattern`,
    $t`a popular provider`,
    $t`company SSO`,
  ]);

  let index = $state(0);
  let shown = $state(true);

  $effect(() => {
    void index;
    const hold = setTimeout(() => {
      shown = false;
    }, 2200);
    return () => clearTimeout(hold);
  });

  $effect(() => {
    if (shown) return;
    const swap = setTimeout(() => {
      index = (index + 1) % words.length;
      shown = true;
    }, 350);
    return () => clearTimeout(swap);
  });
</script>

<div class="relative -mt-2 mb-5 w-full overflow-hidden">
  <div class="halftone absolute -inset-x-20 inset-y-0" aria-hidden="true"></div>
  <div class="relative flex justify-center py-4">
    <PasskeyIllustration class="text-text-primary h-32" />
  </div>
</div>

<h2
  class="text-text-primary mb-1.5 self-center text-center text-[28px] leading-[1.1] font-medium tracking-tight"
>
  {$t`Sign up`}
</h2>

<p
  class="text-text-tertiary mb-7 min-h-11 max-w-[360px] self-center text-center text-[15px] leading-[22px]"
  aria-live="polite"
>
  {$t`Your Internet Identity, with`}
  <span
    class="text-text-primary inline-block font-semibold transition-opacity duration-300"
    style:opacity={shown ? 1 : 0}
  >
    {words[index]}
  </span>
</p>

<style>
  .halftone {
    background-image: radial-gradient(
      circle,
      color-mix(in oklab, var(--fg-primary) 36%, transparent) 1px,
      transparent 1.4px
    );
    background-size: 22px 22px;
    mask-image: radial-gradient(ellipse at center, black 0%, transparent 70%);
    -webkit-mask-image: radial-gradient(
      ellipse at center,
      black 0%,
      transparent 70%
    );
  }
</style>
