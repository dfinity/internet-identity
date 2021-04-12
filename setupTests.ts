import fetch from "isomorphic-fetch";
var util = require("util");
globalThis.TextEncoder = util.TextEncoder;

globalThis.fetch = fetch;
