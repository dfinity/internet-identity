<script lang="ts">
  import { MonitorIcon } from "@lucide/svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Host of the local server being connected (e.g. 127.0.0.1:52341). */
    mcpServerHost: string;
    onContinue: () => void;
    onCancel: () => void;
  }

  const { mcpServerHost, onContinue, onCancel }: Props = $props();
</script>

<!--
  Shown once per identity per computer, before the first sign-in to a local MCP
  server on this machine. A local connector is trusted by host and not by port,
  so this is where the user is told what that means; the consent screen that
  follows still gates this and every later connect.
-->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <FeaturedIcon size="lg" variant="warning" class="mb-5 self-start">
      <MonitorIcon class="size-6" />
    </FeaturedIcon>

    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Allow local programs to connect?`}
    </h1>
    <div class="mt-4 self-start">
      <Badge size="sm" class="max-w-full">
        <Ellipsis text={mcpServerHost} position="middle" />
      </Badge>
    </div>
    <p class="text-text-tertiary mt-4 text-base text-pretty">
      {$t`Any program on this computer can ask. We won't ask again here.`}
    </p>

    <button onclick={onContinue} class="btn btn-primary btn-xl mt-8 w-full">
      {$t`Continue`}
    </button>
    <button onclick={onCancel} class="btn btn-tertiary btn-xl mt-3 w-full">
      {$t`Cancel`}
    </button>
  </AuthPanel>
</div>
