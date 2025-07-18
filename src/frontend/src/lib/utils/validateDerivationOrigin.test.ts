import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { Principal } from "@dfinity/principal";
import { expect } from "vitest";

const FETCH_OPTS = {
  redirect: "error",
  headers: {
    Accept: "application/json",
  },
  credentials: "omit",
};
const TEST_CANISTER_ID = Principal.fromText("bkyz2-fmaaa-aaaaa-qaaaq-cai");

test("should validate no derivation origin", async () => {
  const result = await validateDerivationOrigin({
    requestOrigin: "https://example.com",
    resolveCanisterId: () => Promise.reject("unused in this test"),
  });
  expect(result).toEqual({ result: "valid" });
});

test("should validate same derivation origin", async () => {
  const result = await validateDerivationOrigin({
    requestOrigin: "https://example.com",
    derivationOrigin: "https://example.com",
    resolveCanisterId: () => Promise.reject("unused in this test"),
  });
  expect(result).toEqual({ result: "valid" });
});

test("should fetch alternative origins file from expected URL", async () => {
  const testCases = [
    {
      iiUrl: "https://identity.ic0.app",
      fetchUrl: `https://${TEST_CANISTER_ID}.icp0.io/.well-known/ii-alternative-origins`,
    },
    {
      iiUrl: "https://id.ai",
      fetchUrl: `https://${TEST_CANISTER_ID}.icp0.io/.well-known/ii-alternative-origins`,
    },
    {
      iiUrl: "https://identity.raw.ic0.app",
      fetchUrl: `https://${TEST_CANISTER_ID}.icp0.io/.well-known/ii-alternative-origins`,
    },
    {
      iiUrl: "https://identity.icp0.io",
      fetchUrl: `https://${TEST_CANISTER_ID}.icp0.io/.well-known/ii-alternative-origins`,
    },
    {
      iiUrl: "https://identity.internetcomputer.org",
      fetchUrl: `https://${TEST_CANISTER_ID}.icp0.io/.well-known/ii-alternative-origins`,
    },
    {
      iiUrl: "http://222ew-7aaaa-aaaar-akaia-cai.localhost",
      fetchUrl: `http://localhost/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "http://222ew-7aaaa-aaaar-akaia-cai.localhost:4943",
      fetchUrl: `http://localhost:4943/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "https://222ew-7aaaa-aaaar-akaia-cai.localhost",
      fetchUrl: `https://localhost/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "http://localhost/?canisterId=222ew-7aaaa-aaaar-akaia-cai",
      fetchUrl: `http://localhost/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "https://localhost/?canisterId=222ew-7aaaa-aaaar-akaia-cai",
      fetchUrl: `https://localhost/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "http://0.0.0.0/?canisterId=222ew-7aaaa-aaaar-akaia-cai",
      fetchUrl: `http://0.0.0.0/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "https://0.0.0.0/?canisterId=222ew-7aaaa-aaaar-akaia-cai",
      fetchUrl: `https://0.0.0.0/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "https://0.0.0.0:1234/?canisterId=222ew-7aaaa-aaaar-akaia-cai",
      fetchUrl: `https://0.0.0.0:1234/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "http://127.0.0.1/?canisterId=222ew-7aaaa-aaaar-akaia-cai",
      fetchUrl: `http://127.0.0.1/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
    {
      iiUrl: "https://127.0.0.1/?canisterId=222ew-7aaaa-aaaar-akaia-cai",
      fetchUrl: `https://127.0.0.1/.well-known/ii-alternative-origins?canisterId=${TEST_CANISTER_ID}`,
    },
  ];

  for (const { iiUrl, fetchUrl } of testCases) {
    const fetchMock = setupMocks({
      iiUrl,
      response: Response.json({
        alternativeOrigins: ["https://example.com"],
      }),
    });

    const result = await validateDerivationOrigin({
      requestOrigin: "https://example.com",
      derivationOrigin: "https://some-url.com", // different from requestOrigin so that we need to fetch the alternative origins
      resolveCanisterId: () => Promise.resolve({ ok: TEST_CANISTER_ID }),
    });

    expect(result).toEqual({ result: "valid" });
    expect(fetchMock).toHaveBeenLastCalledWith(fetchUrl, FETCH_OPTS);
  }
});

test("should not validate if canister id resolution fails", async () => {
  const result = await validateDerivationOrigin({
    requestOrigin: "https://example.com",
    derivationOrigin: "https://derivation.com",
    resolveCanisterId: () => Promise.resolve("not_found"),
  });
  expect(result.result).toBe("invalid");
});

const validIIUrls = [
  "https://identity.ic0.app",
  "https://identity.internetcomputer.org",
  "https://id.ai",
];

for (const iiUrl of validIIUrls) {
  test("should fetch alternative origins file using non-raw URL", async () => {
    const fetchMock = setupMocks({
      iiUrl,
      response: Response.json({
        alternativeOrigins: [`https://${TEST_CANISTER_ID}.raw.ic0.app`],
      }),
    });

    const result = await validateDerivationOrigin({
      requestOrigin: `https://${TEST_CANISTER_ID}.raw.ic0.app`,
      derivationOrigin: "https://some-url.com", // different from requestOrigin so that we need to fetch the alternative origins
      resolveCanisterId: () => Promise.resolve({ ok: TEST_CANISTER_ID }),
    });

    expect(result).toEqual({ result: "valid" });
    expect(fetchMock).toHaveBeenLastCalledWith(
      `https://${TEST_CANISTER_ID}.icp0.io/.well-known/ii-alternative-origins`,
      FETCH_OPTS,
    );
  });

  test("should not validate if origin not allowed", async () => {
    setupMocks({
      iiUrl,
      response: Response.json({
        alternativeOrigins: ["https://not-example.com"],
      }),
    });

    const result = await validateDerivationOrigin({
      requestOrigin: "https://example.com",
      derivationOrigin: "https://some-url.com", // different from requestOrigin so that we need to fetch the alternative origins
      resolveCanisterId: () => Promise.resolve({ ok: TEST_CANISTER_ID }),
    });

    expect(result.result).toBe("invalid");
  });

  test("should not validate if alternative origins file malformed", async () => {
    setupMocks({
      iiUrl,
      response: Response.json({
        notAlternativeOrigins: ["https://example.com"],
      }),
    });

    const result = await validateDerivationOrigin({
      requestOrigin: "https://example.com",
      derivationOrigin: "https://some-url.com", // different from requestOrigin so that we need to fetch the alternative origins
      resolveCanisterId: () => Promise.resolve({ ok: TEST_CANISTER_ID }),
    });

    expect(result.result).toBe("invalid");
  });

  test("should not validate on alternative origins redirect", async () => {
    setupMocks({
      iiUrl,
      response: Response.redirect("https://some-evil-url.com"),
    });

    const result = await validateDerivationOrigin({
      requestOrigin: "https://example.com",
      derivationOrigin: "https://some-url.com", // different from requestOrigin so that we need to fetch the alternative origins
      resolveCanisterId: () => Promise.resolve({ ok: TEST_CANISTER_ID }),
    });

    expect(result.result).toBe("invalid");
  });

  test("should not validate on alternative origins error", async () => {
    setupMocks({
      iiUrl,
      response: new Response(undefined, { status: 404 }),
    });

    const result = await validateDerivationOrigin({
      requestOrigin: "https://example.com",
      derivationOrigin: "https://some-url.com", // different from requestOrigin so that we need to fetch the alternative origins
      resolveCanisterId: () => Promise.resolve({ ok: TEST_CANISTER_ID }),
    });

    expect(result.result).toBe("invalid");
  });
}

const setupMocks = ({
  iiUrl,
  response,
}: {
  iiUrl: string;
  response: Response;
}) => {
  // eslint-disable-next-line
  // @ts-ignore
  vi.spyOn(window, "location", "get").mockReturnValue(new URL(iiUrl));
  const fetchMock = vi.fn();
  global.fetch = fetchMock;
  fetchMock.mockReturnValueOnce(Promise.resolve(response));
  return fetchMock;
};
