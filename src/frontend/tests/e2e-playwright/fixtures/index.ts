import { mergeTests } from "@playwright/test";
import { test as inertWorkaroundTest } from "./inertWorkaround";
import { test as identityTest } from "./identity";
import { test as openIdTest } from "./openid";
import { test as ssoTest } from "./sso";
import { test as authorizeTest } from "./authorize";
import { test as attributeConsentViewTest } from "./attributeConsentView";
import { test as recoveryPageTest } from "./recoveryPage";
import { test as managePageTest } from "./managePage";
import { test as manageAccessPageTest } from "./manageAccessPage";
import { test as manageRecoveryPageTest } from "./manageRecoveryPage";

export const test = mergeTests(
  inertWorkaroundTest,
  identityTest,
  openIdTest,
  ssoTest,
  authorizeTest,
  attributeConsentViewTest,
  recoveryPageTest,
  managePageTest,
  manageAccessPageTest,
  manageRecoveryPageTest,
);
