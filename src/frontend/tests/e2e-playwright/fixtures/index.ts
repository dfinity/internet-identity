import { mergeTests } from "@playwright/test";
import { test as identityTest } from "./identity";
import { test as recoveryPhrasePageTest } from "./recovery-phrase-page";

export const test = mergeTests(identityTest, recoveryPhrasePageTest);
