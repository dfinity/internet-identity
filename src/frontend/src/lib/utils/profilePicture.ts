/**
 * Turning a file the user picked into bytes the canister will accept.
 *
 * The canister caps a picture at {@link PROFILE_PICTURE_MAX_BYTES} and stores
 * WebP only. Phone camera output is routinely several megabytes of JPEG, so
 * handing the raw file over would reject almost every real upload. Instead we
 * decode it, downscale to a square, and re-encode as WebP — stepping the
 * quality down until the result fits.
 *
 * The user still picks any common image format; converting is our job. That is
 * what {@link PROFILE_PICTURE_ACCEPT} advertises, and it is deliberately wider
 * than what the canister stores.
 *
 * Everything here runs in the browser, and none of it is a security boundary:
 * the canister re-checks the size and the WebP header on the bytes it
 * receives. This exists to make a 100 KiB cap something a user can actually
 * meet, not to enforce it.
 */

/** Mirrors `PROFILE_PICTURE_MAX_BYTES` on the canister. Keep in sync with
 *  `internet_identity_interface::internet_identity::types::profile_picture`. */
export const PROFILE_PICTURE_MAX_BYTES = 100 * 1024;

/** Edge length of the encoded square, in pixels. 512 is comfortably above
 *  every place the picture is rendered (the largest is a 96px avatar at 3×
 *  device pixel ratio) while leaving room under the byte cap. */
export const PROFILE_PICTURE_EDGE_PX = 512;

/** What the file picker advertises — the formats a user may *choose*, which is
 *  wider than the single format we store. An animated source collapses to its
 *  first frame, which is what `drawImage` gives us. */
export const PROFILE_PICTURE_ACCEPT = "image/png,image/jpeg,image/webp";

/** The only format the canister stores. Mirrors `PROFILE_PICTURE_MEDIA_TYPE`
 *  on the canister side. */
export const PROFILE_PICTURE_MEDIA_TYPE = "image/webp";

/** Upper bound on the file we are willing to decode at all. Decoding is done
 *  by the browser on the main thread, and a deliberately enormous file is a
 *  cheap way to hang the tab — so refuse early rather than trying to
 *  downscale something absurd. */
export const PROFILE_PICTURE_MAX_SOURCE_BYTES = 20 * 1024 * 1024;

export type ProfilePicturePrepareError =
  | { kind: "unsupported-type" }
  | { kind: "source-too-large"; sizeBytes: number; maxBytes: number }
  | { kind: "decode-failed" }
  | { kind: "encode-failed" }
  | { kind: "webp-unsupported" }
  | { kind: "still-too-large"; sizeBytes: number; maxBytes: number };

export class ProfilePictureError extends Error {
  constructor(readonly detail: ProfilePicturePrepareError) {
    super(detail.kind);
    this.name = "ProfilePictureError";
  }
}

/** A picture prepared for upload. */
export interface PreparedProfilePicture {
  /** What `profile_picture_set` receives. */
  bytes: Uint8Array;
  /** For an `<img src>` preview, so the user sees the downscaled result they
   *  are about to store rather than the file they picked. */
  dataUrl: string;
}

/** Quality ladder for the WebP re-encode. Descending, and the last rung is low
 *  enough that a 512×512 photograph always lands under the cap. The first rung
 *  is near-lossless so a flat-colour avatar or a logo — the case a PNG source
 *  used to cover — keeps its crisp edges when it comfortably fits. */
const QUALITY_LADDER = [1, 0.9, 0.8, 0.7, 0.55, 0.4] as const;

const readAsDataUrl = (file: Blob): Promise<string> =>
  new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onerror = () =>
      reject(new ProfilePictureError({ kind: "decode-failed" }));
    reader.onload = () => {
      const { result } = reader;
      if (typeof result !== "string") {
        reject(new ProfilePictureError({ kind: "decode-failed" }));
        return;
      }
      resolve(result);
    };
    reader.readAsDataURL(file);
  });

const decode = async (file: Blob): Promise<HTMLImageElement> => {
  // A `data:` URL rather than `createObjectURL` so there is no handle to
  // revoke — the string is garbage-collected with the element.
  const src = await readAsDataUrl(file);
  const image = new Image();
  image.src = src;
  try {
    await image.decode();
  } catch {
    throw new ProfilePictureError({ kind: "decode-failed" });
  }
  return image;
};

/** Draw `image` centre-cropped into a square canvas of `edge` pixels.
 *
 *  Centre-crop rather than letterbox: the picture is rendered in circular
 *  avatars, where padding would show as dead space around the subject. */
const drawSquare = (
  image: HTMLImageElement,
  edge: number,
): HTMLCanvasElement => {
  const canvas = document.createElement("canvas");
  canvas.width = edge;
  canvas.height = edge;
  const context = canvas.getContext("2d");
  if (context === null) {
    throw new ProfilePictureError({ kind: "encode-failed" });
  }
  const side = Math.min(image.naturalWidth, image.naturalHeight);
  const sourceX = (image.naturalWidth - side) / 2;
  const sourceY = (image.naturalHeight - side) / 2;
  context.imageSmoothingQuality = "high";
  context.drawImage(image, sourceX, sourceY, side, side, 0, 0, edge, edge);
  return canvas;
};

/** Encode `canvas` as WebP.
 *
 *  `toBlob` does not fail on a media type the browser cannot encode — it
 *  silently falls back to PNG (Safari before 16.4 does exactly this for WebP).
 *  Uploading that fallback would be rejected by the canister as "not WebP",
 *  which tells the user nothing, so the produced `blob.type` is checked and a
 *  distinguishable error raised instead. */
const encodeWebp = (
  canvas: HTMLCanvasElement,
  quality: number,
): Promise<Blob> =>
  new Promise((resolve, reject) => {
    canvas.toBlob(
      (blob) => {
        if (blob === null) {
          reject(new ProfilePictureError({ kind: "encode-failed" }));
          return;
        }
        if (blob.type !== PROFILE_PICTURE_MEDIA_TYPE) {
          reject(new ProfilePictureError({ kind: "webp-unsupported" }));
          return;
        }
        resolve(blob);
      },
      PROFILE_PICTURE_MEDIA_TYPE,
      quality,
    );
  });

/**
 * Prepare `file` for `profile_picture_set`.
 *
 * Rejects with a {@link ProfilePictureError} the caller can turn into copy.
 * The happy path always produces bytes within the canister's bounds; a reject
 * with `still-too-large` would mean even the lowest quality rung overshot,
 * which shouldn't happen for a 512×512 image but is reported rather than
 * silently uploading something the canister will refuse.
 */
export const prepareProfilePicture = async (
  file: File,
): Promise<PreparedProfilePicture> => {
  if (!PROFILE_PICTURE_ACCEPT.split(",").includes(file.type)) {
    throw new ProfilePictureError({ kind: "unsupported-type" });
  }
  if (file.size > PROFILE_PICTURE_MAX_SOURCE_BYTES) {
    throw new ProfilePictureError({
      kind: "source-too-large",
      sizeBytes: file.size,
      maxBytes: PROFILE_PICTURE_MAX_SOURCE_BYTES,
    });
  }

  const canvas = drawSquare(await decode(file), PROFILE_PICTURE_EDGE_PX);

  let smallest: Blob | undefined;
  for (const quality of QUALITY_LADDER) {
    const encoded = await encodeWebp(canvas, quality);
    smallest = encoded;
    if (encoded.size <= PROFILE_PICTURE_MAX_BYTES) {
      return toPrepared(encoded);
    }
  }

  throw new ProfilePictureError({
    kind: "still-too-large",
    sizeBytes: smallest?.size ?? 0,
    maxBytes: PROFILE_PICTURE_MAX_BYTES,
  });
};

const toPrepared = async (blob: Blob): Promise<PreparedProfilePicture> => {
  const bytes = new Uint8Array(await blob.arrayBuffer());
  return { bytes, dataUrl: await readAsDataUrl(blob) };
};

/** The `data:` URL for a picture fetched from the canister, so it can be
 *  rendered without another round trip. Mirrors `ProfilePicture::to_data_url`
 *  on the canister side, including the media type — which is a constant on
 *  both sides because WebP is the only format stored. */
export const profilePictureDataUrl = (picture: {
  bytes: Uint8Array | number[];
}): string => {
  const bytes =
    picture.bytes instanceof Uint8Array
      ? picture.bytes
      : new Uint8Array(picture.bytes);
  // Chunked so a 100 KiB picture doesn't blow the argument limit of `apply`.
  let binary = "";
  const CHUNK = 0x8000;
  for (let i = 0; i < bytes.length; i += CHUNK) {
    binary += String.fromCharCode(...bytes.subarray(i, i + CHUNK));
  }
  return `data:${PROFILE_PICTURE_MEDIA_TYPE};base64,${btoa(binary)}`;
};
