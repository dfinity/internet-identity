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
    throw Error(
      `Could not get canister ID for '${canisterName}' with command '${command}', was the canister deployed? ${e}`
    );
  }
};

/** Get the http host of a running replica */
export const getReplicaHost = (): string => {
  const command = `dfx info webserver-port`;
  try {
    const stdout = execSync(command);
    const port = stdout.toString().trim();
    return `http://127.0.0.1:${port}`;
  } catch (e) {
    throw Error(
      `Could not get replica port '${command}', is the replica running? ${e}`
    );
  }
};
