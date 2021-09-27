import canister_ids1 from "../../../../.dfx/local/canister_ids.json";

import fetch from "cross-fetch";
global.fetch = fetch; // "getDefaultFetch" in @dfinity/HttpClient will look for the fetch method here.

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
delete window.location;
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
window.location = new URL("http://localhost:8000"); // in order for the HttpClient to know where the "backend" is, the window location needs to be mocked

process.env.CANISTER_ID = canister_ids1.internet_identity.local;
process.env.II_ENV = "development";
