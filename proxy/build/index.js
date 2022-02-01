#!/usr/bin/env node
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = __importDefault(require("express"));
const morgan_1 = __importDefault(require("morgan"));
const http_proxy_middleware_1 = require("http-proxy-middleware");
// Parse all the arguments, returning an error string if parsing failed.
const parseArgs = (args) => {
    // Ensure '--replica-host http://...' was specified
    const replicaArgIndex = args.indexOf("--replica-host");
    if (replicaArgIndex == -1) {
        return "Please specify --replica-host";
    }
    // Remove the '--replica-host http://...' from the args and store the value
    const replicaArgs = args.splice(replicaArgIndex, 2);
    if (replicaArgs.length != 2) {
        return "No value for --replica-host";
    }
    const [_, replicaHost] = replicaArgs;
    // Parse the rest of the args as canister/port mappings
    const canistersToPorts = parsePortMappings(args);
    if (typeof canistersToPorts === "string") {
        return canistersToPorts;
    }
    else {
        return { replicaHost, canistersToPorts };
    }
};
// Parse the canister ID to port mappings (rwajt-...:8086 rdm6x-...:443 ...)
const parsePortMappings = (args) => {
    const canistersToPorts = {};
    for (const arg of args) {
        const tokens = arg.split(":");
        if (tokens.length != 2) {
            return `Could not parse '${arg}'`;
        }
        const [canisterId, portStr] = tokens;
        const port = parseInt(portStr);
        if (Number.isNaN(port)) {
            return `Could not parse port '${portStr}' as number`;
        }
        canistersToPorts[canisterId] = port;
    }
    return canistersToPorts;
};
// An app that:
// * logs the requests (green for 2XX, blue for 3XX, yellow for 4XX and red for
//      5XX)
// * listens on the specified port and proxies to
//      '<replicaHost>/?canisterId=<canisterId>'
const mkApp = ({ replicaHost, port, canisterId, }) => {
    const app = (0, express_1.default)();
    // could use morgan's .token() thingy but really not worth it here
    app.use((0, morgan_1.default)((_, req, res) => {
        const color = (rc) => {
            if (rc >= 200 && rc < 300) {
                return 32; // GREEN
            }
            else if (rc >= 300 && rc < 400) {
                return 34; // BLUE
            }
            else if (rc >= 400 && rc < 500) {
                return 33; // YELLOW
            }
            else {
                return 35; // RED
            }
        };
        return `${canisterId} (${port}) \x1b[${color(res.statusCode)}m${res.statusCode}\x1b[0m ${req.method} ${req.originalUrl} -> ${req.url} `;
    }));
    app.all("*", (0, http_proxy_middleware_1.createProxyMiddleware)({
        target: replicaHost,
        pathRewrite: (pathAndParams, req) => {
            let queryParamsString = `?`;
            const [path, params] = pathAndParams.split("?");
            if (params) {
                queryParamsString += `${params}&`;
            }
            queryParamsString += `canisterId=${canisterId}`;
            return path + queryParamsString;
        },
    }));
    app.listen(port, "localhost", () => {
        console.log(`Canister ${canisterId} is listening on http://localhost:${port}`);
    });
};
const usage = "USAGE: proxy --replica-host http://... [<canister-id>:<port>]";
const args = process.argv.slice(2);
if (args.indexOf("--help") != -1) {
    console.log(usage);
    process.exit(0);
}
const parsed = parseArgs(args);
if (typeof parsed === "string") {
    console.log(parsed);
    console.log(usage);
    process.exit(1);
}
if (Object.keys(parsed.canistersToPorts).length == 0) {
    console.log("No canisters to proxy");
    console.log(usage);
    process.exit(1);
}
for (const canisterId in parsed.canistersToPorts) {
    const port = parsed.canistersToPorts[canisterId];
    console.log(`Forwarding ${port} to ${parsed.replicaHost}/?canisterId=${canisterId}`);
    mkApp({ replicaHost: parsed.replicaHost, port, canisterId });
}
