<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
</script>

<div class="relative -mt-2 mb-8 w-full overflow-hidden">
  <div class="relative flex justify-center py-4">
    <PasskeyIllustration class="text-text-primary h-32" />
  </div>
</div>

<h2
  class="text-text-primary mb-1.5 self-center text-center text-[28px] leading-[1.1] font-medium tracking-tight"
>
  {$t`Create new identity`}
</h2>

<p
  class="text-text-tertiary mb-7 max-w-[360px] self-center text-center text-[15px] leading-[22px]"
>
  {$t`Set up your private, passwordless identity.`}
</p>
