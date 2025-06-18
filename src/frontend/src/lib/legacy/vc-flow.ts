import { vcFlow } from "$lib/legacy/flows/verifiableCredentials";
import { createSpa } from "./spa";
import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";

void createSpa((connection) => {
  initAnalytics(connection.canisterConfig.analytics_config[0]?.[0]);
  analytics.pageView();
  return vcFlow({ connection });
});
