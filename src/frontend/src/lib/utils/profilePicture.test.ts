import {
  PROFILE_PICTURE_MAX_SOURCE_BYTES,
  ProfilePictureError,
  prepareProfilePicture,
  profilePictureDataUrl,
} from "./profilePicture";

describe("profilePictureDataUrl", () => {
  it("renders each media type with its IANA name", () => {
    const bytes = new Uint8Array([1, 2, 3]);
    expect(profilePictureDataUrl({ media_type: { Png: null }, bytes })).toMatch(
      /^data:image\/png;base64,/,
    );
    expect(
      profilePictureDataUrl({ media_type: { Jpeg: null }, bytes }),
    ).toMatch(/^data:image\/jpeg;base64,/);
    expect(
      profilePictureDataUrl({ media_type: { Webp: null }, bytes }),
    ).toMatch(/^data:image\/webp;base64,/);
  });

  // The canister builds the same string from the same bytes
  // (`ProfilePicture::to_data_url`); this pins the encoding both sides agree
  // on, so a picture pinned as `spec.value` still matches at certification.
  it("matches the canister's encoding of the same bytes", () => {
    expect(
      profilePictureDataUrl({
        media_type: { Png: null },
        bytes: new Uint8Array([0x89, 0x50, 0x4e, 0x47]),
      }),
    ).toBe("data:image/png;base64,iVBORw==");
  });

  it("accepts `number[]` bytes (the Candid wire shape)", () => {
    expect(
      profilePictureDataUrl({
        media_type: { Png: null },
        bytes: [0x89, 0x50, 0x4e, 0x47],
      }),
    ).toBe("data:image/png;base64,iVBORw==");
  });

  // A 100 KiB picture is well past the argument limit of `String.fromCharCode`
  // applied in one go, which is why the encoder chunks. Guard the chunking.
  it("encodes a picture at the storage cap without blowing the call stack", () => {
    const bytes = new Uint8Array(100 * 1024).fill(0xab);
    const url = profilePictureDataUrl({ media_type: { Jpeg: null }, bytes });
    // base64 is 4 characters per 3 bytes, rounded up to a whole quantum.
    const expectedBase64Length = 4 * Math.ceil(bytes.length / 3);
    expect(url.length).toBe(
      "data:image/jpeg;base64,".length + expectedBase64Length,
    );
  });
});

describe("prepareProfilePicture", () => {
  // The guards below run before any canvas work, so they are testable under
  // jsdom. The decode/downscale/re-encode path needs a real canvas
  // implementation and is covered end to end in the browser instead.
  const fileOf = (type: string, size: number): File => {
    const file = new File([new Uint8Array(0)], "avatar", { type });
    // `File` derives `size` from its contents, and allocating 20 MB to test a
    // size guard is wasteful — override the getter instead.
    Object.defineProperty(file, "size", { value: size });
    return file;
  };

  it("rejects a media type the canister would not accept", async () => {
    await expect(
      prepareProfilePicture(fileOf("image/gif", 1024)),
    ).rejects.toMatchObject({
      name: "ProfilePictureError",
      detail: { kind: "unsupported-type" },
    });
  });

  it("rejects an SVG, which is a script vector rather than an image", async () => {
    await expect(
      prepareProfilePicture(fileOf("image/svg+xml", 1024)),
    ).rejects.toMatchObject({ detail: { kind: "unsupported-type" } });
  });

  it("refuses to decode a source large enough to hang the tab", async () => {
    const size = PROFILE_PICTURE_MAX_SOURCE_BYTES + 1;
    await expect(
      prepareProfilePicture(fileOf("image/jpeg", size)),
    ).rejects.toMatchObject({
      detail: {
        kind: "source-too-large",
        sizeBytes: size,
        maxBytes: PROFILE_PICTURE_MAX_SOURCE_BYTES,
      },
    });
  });

  it("reports failures as ProfilePictureError so the UI can map them to copy", async () => {
    await expect(
      prepareProfilePicture(fileOf("application/pdf", 10)),
    ).rejects.toBeInstanceOf(ProfilePictureError);
  });
});
