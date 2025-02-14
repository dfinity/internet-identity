<script lang="ts">
  import { authnTemplateManage } from "$src/flows/manage";
  import { Connection } from "$src/utils/iiConnection.js";
  import { readCanisterConfig, readCanisterId } from "$src/spa";
  import { I18n } from "$src/i18n";
  import Flow from "../../lib/Flow.svelte";
  import { shuffleArray } from "$src/utils/utils";
  import { getDapps } from "$src/flows/dappsExplorer/dapps";
  import { ENABLE_PIN_QUERY_PARAM_KEY } from "$src/config";
  import { authenticateBox } from "$src/components/authenticateBox";
  import { authenticationState } from "../../lib/authentication.svelte.js";
  import { page } from "$app/state";
  import { goto } from "$app/navigation";

  const i18n = new I18n();
  const dapps = shuffleArray(getDapps());
  const templates = authnTemplateManage({ dapps });
  const allowPinRegistration = page.url.searchParams.get(ENABLE_PIN_QUERY_PARAM_KEY) !== null;
  const connection = new Connection(readCanisterId(), readCanisterConfig());

  $effect(() => {
    if (authenticationState.connection) {
      goto(page.url.searchParams.get("next") ?? "/");
    }
  });
</script>


{#if !authenticationState.connection}
  <Flow
    promise={authenticateBox}
    args={[
      {
        connection,
        i18n,
        templates,
        allowPinLogin: true,
        allowPinRegistration,
      },
    ]}
    onResolve={({connection}) => authenticationState.connection = connection}
  />
{/if}
