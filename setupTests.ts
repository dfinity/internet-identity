import fetch from "isomorphic-fetch";
var util = require("util");
globalThis.TextEncoder = util.TextEncoder;

globalThis.fetch = fetch;

globalThis.mockActor = {
  register: jest.fn(),
  add: jest.fn(),
  remove: jest.fn(),
  lookup: jest.fn(),
};

export const testGlobal: any = { ...globalThis };

jest.mock("@dfinity/agent", () => {
  class HttpAgent {}
  return {
    HttpAgent,
    Actor: {
      createActor: () => globalThis.mockActor,
    },
  };
});
