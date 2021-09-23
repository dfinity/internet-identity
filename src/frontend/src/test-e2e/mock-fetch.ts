import fetch from 'cross-fetch';
global.fetch = fetch;

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
delete window.location
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
window.location = new URL('http://localhost:8080');

process.env.CANISTER_ID = 'rwlgt-iiaaa-aaaaa-aaaaa-cai';
process.env.II_ENV = 'development';
