import { mergeTests } from "@playwright/test";
import { test as identityTest } from "./identity";
import { test as recoveryPageTest } from "./recoveryPage";
import { test as managePageTest } from "./managePage";
import { test as manageAccessPageTest } from "./manageAccessPage";
import { test as manageRecoveryPageTest } from "./manageRecoveryPage";

export const test = mergeTests(
  identityTest,
  recoveryPageTest,
  managePageTest,
  manageAccessPageTest,
  manageRecoveryPageTest,
);
