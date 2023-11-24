/* Utils used both in the FE code & the FE config */

import { execSync } from "child_process";

/**
 * Read a canister ID from dfx's local state
 */
export const readCanisterId = ({
  canisterName,
}: {
  canisterName: string;
}): string => {
  const command = `dfx canister id ${canisterName}`;
  try {
    const stdout = execSync(command);
    return stdout.toString().trim();
  } catch (e) {
    throw Error(`Could not get canister ID with command '${command}': ${e}`);
  }
};
