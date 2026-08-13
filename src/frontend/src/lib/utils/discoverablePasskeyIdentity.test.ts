import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";

describe("DiscoverablePasskeyIdentity", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  describe("sign", () => {
    it("aborts the ceremony still in flight when a new one starts", () => {
      const signals: (AbortSignal | undefined)[] = [];
      const get = vi.fn((options: CredentialRequestOptions) => {
        signals.push(options.signal);
        // A pending ceremony: the platform settles it once the user picks a
        // passkey, so nothing resolves this on its own.
        return new Promise<Credential | null>(() => {});
      });
      vi.stubGlobal("navigator", { credentials: { get } });
      const identity = new DiscoverablePasskeyIdentity({
        credentialRequestOptions: {
          publicKey: { userVerification: "required" },
        },
      });

      void identity.sign(new Uint8Array([1]));
      void identity.sign(new Uint8Array([2]));

      expect(signals).toHaveLength(2);
      expect(signals[0]?.aborted).toBe(true);
      expect(signals[1]?.aborted).toBe(false);
    });
  });
});
