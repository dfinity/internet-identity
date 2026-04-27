<script lang="ts">
  import Flow from "$lib/components/utils/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { vcFlow } from "$lib/legacy/flows/verifiableCredentials";
  import { frontendCanisterConfig, canisterId } from "$lib/globals";

  analytics.event("page-webauthn-iframe");

  const connection = new Connection(canisterId.toText(), {
    is_production: [],
    backend_canister_id: [frontendCanisterConfig.backend_canister_id],
    enable_dapps_explorer: [],
    assigned_user_number_range: [],
    new_flow_origins: [],
    archive_config: [],
    canister_creation_cycles_cost: [],
    analytics_config: [],
    related_origins: frontendCanisterConfig.related_origins,
    openid_configs: [],
    sso_discoverable_domains: [],
    backend_origin: [frontendCanisterConfig.backend_origin],
    captcha_config: [],
    dummy_auth: [],
    register_rate_limit: [],
  });
</script>

<Flow promise={vcFlow} args={[{ connection }]} />
