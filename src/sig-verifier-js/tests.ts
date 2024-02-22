import { verifyIcSignature } from "@dfinity/sig-verifier-js/sig_verifier_js";

// This just checks that the Wasm can be successfully loaded.
test("Wasm can be loaded", async () => {
  try {
    await verifyIcSignature(
      Uint8Array.from([1, 2, 3, 5]),
      Uint8Array.from([1, 2, 3, 5]),
      Uint8Array.from([1, 2, 3, 5]),
      Uint8Array.from([1, 2, 3, 5])
    );
  } catch (e: unknown) {
    // Expect an error string generated from the Wasm module
    expect(e).toContain(
      "Error in DER encoding: Bad length field in boolean block: 2"
    );
  }
});
