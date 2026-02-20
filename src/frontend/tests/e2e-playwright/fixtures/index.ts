import { mergeTests } from "@playwright/test";
import { test as identityTest } from "./identity";
import { test as openIdTest } from "./openid";
import { test as authorizeTest } from "./authorize";
import { test as recoveryPageTest } from "./recoveryPage";
import { test as manageRecoveryPageTest } from "./manageRecoveryPage";

export const test = mergeTests(
  identityTest,
  openIdTest,
  authorizeTest,
  recoveryPageTest,
  manageRecoveryPageTest,
);
