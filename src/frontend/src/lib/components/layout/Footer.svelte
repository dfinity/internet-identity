<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { SOURCE_CODE_URL, SUPPORT_URL } from "$lib/config";
  import { t } from "$lib/stores/locale.store";

  type Props = HTMLAttributes<HTMLElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<footer
  {...props}
  class={[
    "flex h-14 items-center justify-between px-4 py-3 md:px-6 lg:px-8",
    className,
  ]}
>
  <div class="text-text-tertiary text-xs font-medium">© Internet Identity</div>
  <nav class="text-text-primary flex gap-4 text-xs font-semibold">
    <a
      href={SUPPORT_URL}
      target="_blank"
      rel="noopener"
      class="outline-0 focus-visible:underline"
    >
      {$t`Support`}
    </a>
    <a
      href={SOURCE_CODE_URL}
      target="_blank"
      rel="noopener"
      class="outline-0 focus-visible:underline"
    >
      {$t`Source code`}
    </a>
  </nav>
</footer>
