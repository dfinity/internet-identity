import { resolveCanisterId } from "$lib/utils/canisterIdResolution";
import { Principal } from "@dfinity/principal";

const HEADER_NAME = "x-ic-canister-id";

test("should resolve canister id", async () => {
  const fetchMock = vi.fn();
  fetchMock.mockReturnValueOnce(
    new Response(null, {
      status: 200,
      headers: [[HEADER_NAME, "bkyz2-fmaaa-aaaaa-qaaaq-cai"]],
    }),
  );
  global.fetch = fetchMock;

  expect(
    await resolveCanisterId({
      origin: "https://example.com",
    }),
  ).toEqual({ ok: Principal.fromText("bkyz2-fmaaa-aaaaa-qaaaq-cai") });
});

test("should not resolve canister id on missing header", async () => {
  const fetchMock = vi.fn();
  fetchMock.mockReturnValueOnce(
    new Response(null, {
      status: 200,
    }),
  );
  global.fetch = fetchMock;

  expect(
    await resolveCanisterId({
      origin: "https://example.com",
    }),
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
      }),
    ).toEqual({ ok: Principal.fromText(canisterId) });
    expect(
      await resolveCanisterId({
        origin: `https://${canisterId}.raw.${domain}`,
      }),
    ).toEqual({ ok: Principal.fromText(canisterId) });
  }
  // make sure fetch was not called
  expect(global.fetch).toHaveBeenCalledTimes(0);
});

test("should not resolve canister id from malformed header", async () => {
  const fetchMock = vi.fn();
  fetchMock.mockReturnValueOnce(
    new Response(null, {
      status: 200,
      headers: [[HEADER_NAME, "not_a_canister_id"]],
    }),
  );
  global.fetch = fetchMock;

  expect(
    await resolveCanisterId({
      origin: "https://example.com",
    }),
  ).toEqual("not_found");
});

test("should resolve canister id from a custom domain that returns a response with status 500", async () => {
  const fetchMock = vi.fn();
  const canisterId = "bkyz2-fmaaa-aaaaa-qaaaq-cai";
  fetchMock.mockReturnValueOnce(
    new Response(null, {
      status: 500,
      // Boundary Node sets the header even on error
      headers: [[HEADER_NAME, canisterId]],
    }),
  );
  global.fetch = fetchMock;

  expect(
    await resolveCanisterId({
      origin: "https://example.com",
    }),
  ).toEqual({ ok: Principal.fromText(canisterId) });
});

test("should not resolve canister id from malformed origin", async () => {
  expect(
    await resolveCanisterId({
      origin: "not-an-origin",
    }),
  ).toEqual("not_found");
});
