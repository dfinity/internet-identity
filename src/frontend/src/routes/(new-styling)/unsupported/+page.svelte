<script lang="ts">
  import { SUPPORT_URL } from "$lib/config";
  import Header from "$lib/components/layout/Header.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
</script>

<div class="flex min-h-[100dvh] flex-col" data-page="new-unsupported-view">
  <div class="h-[env(safe-area-inset-top)]"></div>

  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <div
      class="flex flex-1 flex-col items-stretch justify-end p-4 sm:max-w-100 sm:items-center sm:justify-center"
    >
      <div class="mb-8 flex flex-1 flex-col justify-center sm:flex-none">
        <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
          <TriangleAlertIcon size="1.25rem" class="text-fg-warning-primary" />
        </FeaturedIcon>
        <h1 class="text-text-primary mb-3 text-left text-2xl font-medium">
          {$t`Unsupported Browser`}
        </h1>
        <p class="text-text-primary mb-5 text-left">
          {$t`It looks like you're trying to sign in from inside an app (such as X, Telegram, or Instagram). This app's built-in browser is not supported.`}
        </p>
        <div>
          <p class="text-text-primary mb-3 text-left">
            <strong>{$t`To continue:`}</strong>
          </p>
          <ol
            class="text-md text-text-tertiary mb-5 list-decimal space-y-2 pl-6 text-left font-medium"
          >
            <li>{$t`Long-press the link that opened this page.`}</li>
            <li>
              <Trans>
                Select <span class="text-text-primary font-semibold"
                  >Open in Safari</span
                >
                or
                <span class="text-text-primary font-semibold"
                  >Open in Chrome</span
                >.
              </Trans>
            </li>
            <li>{$t`Sign in there.`}</li>
          </ol>
        </div>
        <p class="text-center">
          <a
            href={SUPPORT_URL}
            target="_blank"
            rel="noopener noreferrer"
            class="text-text-primary underline hover:no-underline"
            >{$t`Learn how to open links in your browser →`}</a
          >
        </p>
      </div>
    </div>
  </div>
</div>
