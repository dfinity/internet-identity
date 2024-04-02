import { resolveCanisterId } from "$src/utils/canisterIdResolution";

const HEADER_NAME = "x-ic-canister-id";

test("should resolve canister id", async () => {
  const fetchMock = vi.fn();
  fetchMock.mockReturnValueOnce(
    new Response(null, {
      status: 200,
      headers: [[HEADER_NAME, "bkyz2-fmaaa-aaaaa-qaaaq-cai"]],
    })
  );
  global.fetch = fetchMock;

  expect(
    await resolveCanisterId({
      origin: "https://example.com",
    })
  ).toEqual({ ok: "bkyz2-fmaaa-aaaaa-qaaaq-cai" });
});

test("should not resolve canister id on missing header", async () => {
  const fetchMock = vi.fn();
  fetchMock.mockReturnValueOnce(
    new Response(null, {
      status: 200,
    })
  );
  global.fetch = fetchMock;

  expect(
    await resolveCanisterId({
      origin: "https://example.com",
    })
  ).toEqual("not_found");
});

test("should resolve canister id from well-known domain", async () => {
  global.fetch = vi.fn();
  const canisterId = "bkyz2-fmaaa-aaaaa-qaaaq-cai";
  const wellKnownDomains = [
    "ic0.app",
    "icp0.io",
    "internetcomputer.org",
    "localhost",
  ];

  for (const domain of wellKnownDomains) {
    expect(
      await resolveCanisterId({
        origin: `https://${canisterId}.${domain}`,
      })
    ).toEqual({ ok: canisterId });
    expect(
      await resolveCanisterId({
        origin: `https://${canisterId}.raw.${domain}`,
      })
    ).toEqual({ ok: canisterId });
  }
  // make sure fetch was not called
  expect(global.fetch).toHaveBeenCalledTimes(0);
});
