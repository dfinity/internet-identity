import { getContext, setContext } from "svelte";
import type { AuthenticatedConnection } from "$src/utils/iiConnection";

const connectionKey = Symbol("connection");

export function setConnectionContext(connection: AuthenticatedConnection) {
  setContext(connectionKey, connection);
}

export function getConnectionContext(): AuthenticatedConnection {
  return getContext(connectionKey) as AuthenticatedConnection;
}
