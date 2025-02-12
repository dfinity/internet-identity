import { vcFlow } from "./flows/verifiableCredentials";
import { createSpa } from "./spa";
import { analytics } from "./utils/analytics";

void createSpa((connection) => {
  analytics.pageView();
  return vcFlow({ connection });
});
