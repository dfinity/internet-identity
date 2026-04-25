/* Utils used both in the FE code & the FE config */

import { execSync } from "child_process";
import type { IncomingMessage, ServerResponse } from "http";
import { request } from "undici";

export const readReplicaPort = (): string => {
  const stdout = execSync("icp network status --json");
  const status = JSON.parse(stdout.toString());
  return new URL(status.gateway_url).port;
};

export const readCanisterId = ({
  canisterName,
}: {
  canisterName: string;
}): string => {
  const command = `icp canister status ${canisterName} --id-only`;
  try {
    const stdout = execSync(command);
    return stdout.toString().trim();
  } catch (e) {
    throw Error(
      `Could not get canister ID for '${canisterName}' with command '${command}', was the canister deployed? ${e}`,
    );
  }
};

export const getReplicaHost = (): string => {
  try {
    const stdout = execSync("icp network status --json");
    const status = JSON.parse(stdout.toString());
    const port = new URL(status.gateway_url).port;
    return `http://127.0.0.1:${port}`;
  } catch (e) {
    throw Error(`Could not get replica port, is the replica running? ${e}`);
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
  const authority = req.headers[":authority"] as string;
  console.log(
    `forwarding ${req.method} https://${authority}${req.url} to canister ${canisterId} ${replicaOrigin}`,
  );

  // Start by crafting the new request with the original request's headers
  const reqHeaders: string[] = [];
  for (const k in req.headers) {
    if (k.startsWith(":")) {
      // Skip pseudo-headers, we add the authority manually later
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
