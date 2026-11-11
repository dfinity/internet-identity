import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { describe, expect, it } from "vitest";
import {
  exportLinkKey,
  generateLinkKey,
  importLinkKey,
  LINK_KEY_BYTES,
} from "./linkKey";

describe("link key", () => {
  it("writes the key in the length the fragment is read with", async () => {
    const written = await exportLinkKey(await generateLinkKey());

    expect(written).toHaveLength(LINK_KEY_BYTES);
  });

  // The canister matches the caller against the principal the browser registered, so the
  // key read out of the URL has to resolve to the one that was put in.
  it("comes back as the identity it was minted as", async () => {
    const keyPair = await generateLinkKey();
    const minted = await ECDSAKeyIdentity.fromKeyPair(keyPair);

    const carried = await ECDSAKeyIdentity.fromKeyPair(
      await importLinkKey(await exportLinkKey(keyPair)),
    );

    expect(carried.getPrincipal().toText()).toBe(
      minted.getPrincipal().toText(),
    );
  });

  it("comes back able to sign for that identity", async () => {
    const keyPair = await generateLinkKey();
    const carried = await importLinkKey(await exportLinkKey(keyPair));
    const message = new TextEncoder().encode("ii-link-key-round-trip");

    const signature = await crypto.subtle.sign(
      { name: "ECDSA", hash: "SHA-256" },
      carried.privateKey,
      message,
    );

    await expect(
      crypto.subtle.verify(
        { name: "ECDSA", hash: "SHA-256" },
        keyPair.publicKey,
        signature,
        message,
      ),
    ).resolves.toBe(true);
  });

  it("mints a key of its own every time", async () => {
    const [one, other] = await Promise.all([
      generateLinkKey().then(exportLinkKey),
      generateLinkKey().then(exportLinkKey),
    ]);

    expect(one).not.toEqual(other);
  });
});
