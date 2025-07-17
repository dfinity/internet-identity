/* Utils used both in the FE code & the FE config */

import { execSync } from "child_process";
import type { IncomingMessage, ServerResponse } from "http";
import { request } from "undici";

/**
 * Read the replica port from dfx's local state
 */
export const readReplicaPort = (): string => {
  const stdout = execSync("dfx info webserver-port");
  return stdout.toString().trim();
};

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
      `Could not get canister ID for '${canisterName}' with command '${command}', was the canister deployed? ${e}`,
    );
  }
};

/**
 * Read a canister config from dfx's local state
 */
export const readCanisterConfig = ({
  canisterName,
}: {
  canisterName: string;
}): string => {
  const command = `dfx canister call ${canisterName} config --output raw`;
  try {
    const stdout = execSync(command);
    return Buffer.from(stdout.toString().trim(), "hex").toString("base64");
  } catch (e) {
    throw Error(
      `Could not get canister config for '${canisterName}' with command '${command}', was the canister deployed? ${e}`,
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
      `Could not get replica port '${command}', is the replica running? ${e}`,
    );
  }
};

// Forward a request (IncomingMessage) to a canister `canisterId` installed on a replica listening at `replicaOrigin`.
export const forwardToReplica = async ({
  canisterId,
  res,
  req,
  replicaOrigin,
}: {
  canisterId: string;
  res: ServerResponse;
  req: IncomingMessage;
  replicaOrigin: string;
}) => {
  console.log(
    `forwarding ${req.method} https://${req.headers.host}${req.url} to canister ${canisterId} ${replicaOrigin}`,
  );

  // Start by crafting the new request with the original request's headers
  const reqHeaders: string[] = [];
  for (const k in req.headers) {
    if (k.match(/host/i)) {
      // Skip the host header, we add it manually later
      continue;
    }

    reqHeaders.push(k);
    reqHeaders.push(homogenizeHeaderValue(req.headers[k]));
  }

  // Set the host in a way that icx-proxy knows how to proxy to the right canister
  reqHeaders.push(`host`);
  reqHeaders.push(`${canisterId}.localhost`);

  // XXX: we use undici (used also by Vite) instead of node fetch because node fetch does
  // not support setting a 'host' header
  const proxyResp = await request(`http://${replicaOrigin}${req.url}`, {
    method: req.method as any /* undici doesn't trust 'string' */,
    headers: reqHeaders,
    body: req,
  });

  // Read the status code & headers from the response
  res.statusCode = proxyResp.statusCode;

  const respHeaders = proxyResp.headers;
  for (const k in respHeaders) {
    res.setHeader(k, homogenizeHeaderValue(respHeaders[k]));
  }

  // Add a 'x-ic-canister-id' header like the BNs do
  res.setHeader("x-ic-canister-id", canisterId);

  // Ensure the browser accepts the response
  res.setHeader("access-control-allow-origin", "*");
  res.setHeader("access-control-expose-headers", "*");
  res.setHeader("access-control-allow-headers", "*");

  res.end(new Uint8Array(await proxyResp.body.arrayBuffer()));
};

// Convert a header value to a string
const homogenizeHeaderValue = (
  header: string | string[] | undefined,
): string => {
  if (header === undefined) {
    return "";
  }

  if (typeof header === "string") {
    return header;
  }

  return header.join("; ");
};
