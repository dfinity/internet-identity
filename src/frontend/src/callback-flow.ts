import { callbackFlow } from "$src/flows/redirect";
import { analytics, initAnalytics } from "$src/utils/analytics";
import { createSpa } from "./spa";

void createSpa((connection) => {
  initAnalytics(connection.canisterConfig.analytics_config[0]?.[0]);
  analytics.pageView();
  analytics.event("page-redirect-callback");
  return callbackFlow();
});
