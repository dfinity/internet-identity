import { AuthenticatedConnection } from "$src/utils/iiConnection";

export const authenticationState = $state<{
  connection?: AuthenticatedConnection
}>({});