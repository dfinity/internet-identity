<script lang="ts">
  import Flow from "$lib/components/Flow.svelte";
  import { AuthenticatedConnection, Connection } from "$lib/utils/iiConnection";
  import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
  import { getAddDeviceAnchor } from "$lib/utils/addDeviceLink";
  import { analytics } from "$lib/utils/analytics";
  import { registerTentativeDevice } from "$lib/flows/addDevice/welcomeView/registerTentativeDevice";
  import { goto } from "$app/navigation";
  import { handleLoginFlowResult } from "$lib/templates/authenticateBox";
  import { nonNullish } from "@dfinity/utils";
  import { renderManage } from "$lib/flows/manage";
  import identityCardBackground from "$lib/legacy/assets/identityCardBackground.png?url";
  import { PreLoadImage } from "$lib/utils/preLoadImage";

  analytics.event("page-add-new-device");

  let authenticatedConnection = $state<AuthenticatedConnection | undefined>(
    undefined,
  );

  const addDeviceAnchor = getAddDeviceAnchor(new URL(window.location.href))!;
  const connection = new Connection(readCanisterId(), readCanisterConfig());
  const onResolve = async (
    registerDeviceResult: Awaited<ReturnType<typeof registerTentativeDevice>>,
  ) => {
    if (registerDeviceResult.tag === "canceled") {
      goto("/");
      return;
    }
    void (registerDeviceResult satisfies { tag: "deviceAdded" });

    // If user "Click" continue in success page, proceed with authentication
    const result = await connection.login(addDeviceAnchor);
    const loginData = await handleLoginFlowResult(result);

    // User have successfully signed-in we can jump to manage page
    if (nonNullish(loginData)) {
      authenticatedConnection = loginData.connection;
    }
  };
</script>

{#if nonNullish(authenticatedConnection)}
  <!-- TODO: Once authentication is actual svelte state, this can be removed -->
  <Flow
    promise={renderManage}
    args={[
      {
        userNumber: addDeviceAnchor,
        connection: authenticatedConnection,
        identityBackground: new PreLoadImage(identityCardBackground),
      },
    ]}
  />
{:else}
  <Flow
    promise={registerTentativeDevice}
    args={[addDeviceAnchor, connection]}
    {onResolve}
  />
{/if}
