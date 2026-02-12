import { callbackFlow } from "$lib/legacy/flows/redirect";
import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";
import { createSpa } from "./spa";

void createSpa((connection) => {
  initAnalytics(connection.canisterConfig.analytics_config[0]?.[0]);
  analytics.event("page-redirect-callback");
  return callbackFlow();
});
