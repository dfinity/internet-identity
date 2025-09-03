<script lang="ts">
  export let header: string;
  export let subheader: string;
  export let description: string;
</script>

<div
  class="border-bg-disabled from-bg-primary_alt to-bg-secondary flex max-w-[394px] flex-col rounded-xl border bg-gradient-to-b px-6 pt-5 pb-8"
>
  <p class="text-text-tertiary text-sm uppercase">{header}</p>
  <div class="grid h-[280px] place-items-center">
    <slot name="illustration" />
  </div>
  <div class="flex flex-col gap-2">
    <p class="text-text-primary text-xl">{subheader}</p>
    <p class="text-text-tertiary text-base">{description}</p>
  </div>
</div>
