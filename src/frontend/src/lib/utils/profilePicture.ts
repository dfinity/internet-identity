/**
 * Turning a file the user picked into bytes the canister will accept.
 *
 * The canister caps a picture at {@link PROFILE_PICTURE_MAX_BYTES} and accepts
 * only PNG, JPEG and WebP, sniffed from the magic number. Phone camera output
 * is routinely several megabytes, so handing the raw file over would reject
 * almost every real upload. Instead we decode it, downscale to a square
 * thumbnail, and re-encode — stepping the quality down until the result fits.
 *
 * Everything here runs in the browser, and none of it is a security boundary:
 * the canister re-checks the size and re-derives the media type from the bytes
 * it receives. This exists to make a 100 KiB cap something a user can actually
 * meet, not to enforce it.
 */

import type { ProfilePictureMediaType } from "$lib/generated/internet_identity_types";

/** Mirrors `PROFILE_PICTURE_MAX_BYTES` on the canister. Keep in sync with
 *  `internet_identity_interface::internet_identity::types::profile_picture`. */
export const PROFILE_PICTURE_MAX_BYTES = 100 * 1024;

/** Edge length of the encoded square, in pixels. 512 is comfortably above
 *  every place the picture is rendered (the largest is a 96px avatar at 3×
 *  device pixel ratio) while leaving room under the byte cap. */
export const PROFILE_PICTURE_EDGE_PX = 512;

/** What the file picker advertises. WebP is encodable everywhere we run, so a
 *  user can also supply one directly; the animated case collapses to its first
 *  frame, which is what `drawImage` gives us. */
export const PROFILE_PICTURE_ACCEPT = "image/png,image/jpeg,image/webp";

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
  mediaType: string;
}

/** Quality ladder for the lossy re-encode. Descending, and the last rung is
 *  low enough that a 512×512 photograph always lands under the cap. */
const QUALITY_LADDER = [0.9, 0.8, 0.7, 0.55, 0.4] as const;

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

const encode = (
  canvas: HTMLCanvasElement,
  mediaType: string,
  quality?: number,
): Promise<Blob> =>
  new Promise((resolve, reject) => {
    canvas.toBlob(
      (blob) => {
        if (blob === null) {
          reject(new ProfilePictureError({ kind: "encode-failed" }));
          return;
        }
        resolve(blob);
      },
      mediaType,
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

  // PNG is lossless and has no quality knob, so a re-encoded photograph can
  // easily exceed the cap. Keep PNG only when the source was PNG *and* it
  // fits — that preserves crisp logos and flat-colour avatars — and fall back
  // to JPEG otherwise.
  if (file.type === "image/png") {
    const png = await encode(canvas, "image/png");
    if (png.size <= PROFILE_PICTURE_MAX_BYTES) {
      return toPrepared(png, "image/png");
    }
  }

  let smallest: Blob | undefined;
  for (const quality of QUALITY_LADDER) {
    const encoded = await encode(canvas, "image/jpeg", quality);
    smallest = encoded;
    if (encoded.size <= PROFILE_PICTURE_MAX_BYTES) {
      return toPrepared(encoded, "image/jpeg");
    }
  }

  throw new ProfilePictureError({
    kind: "still-too-large",
    sizeBytes: smallest?.size ?? 0,
    maxBytes: PROFILE_PICTURE_MAX_BYTES,
  });
};

const toPrepared = async (
  blob: Blob,
  mediaType: string,
): Promise<PreparedProfilePicture> => {
  const bytes = new Uint8Array(await blob.arrayBuffer());
  return { bytes, dataUrl: await readAsDataUrl(blob), mediaType };
};

/** The IANA media type for a candid `ProfilePictureMediaType`.
 *
 *  Every variant is matched explicitly and the fall-through is a compile
 *  error, not a guess: if the canister gains a format and the generated
 *  bindings are regenerated without updating this function, `variant` is no
 *  longer `never` and `npm run check` fails. Were this a chain ending in a
 *  default, a new format would instead be silently mislabelled — and the
 *  label is what a relying party's `<img>` tag trusts.
 *
 *  The throw is the runtime half of the same guard, for a value that reached
 *  us without passing through the type system. Callers render the picture
 *  inside an `{#await}`, so it surfaces as the placeholder rather than as an
 *  image with the wrong MIME type. */
const ianaMediaType = (variant: ProfilePictureMediaType): string => {
  if ("Png" in variant) return "image/png";
  if ("Jpeg" in variant) return "image/jpeg";
  if ("Webp" in variant) return "image/webp";
  const unhandled: never = variant;
  throw new Error(
    `Unknown profile picture media type: ${JSON.stringify(unhandled)}`,
  );
};

/** The `data:` URL for a picture fetched from the canister, so it can be
 *  rendered without another round trip. Mirrors `ProfilePicture::to_data_url`
 *  on the canister side. */
export const profilePictureDataUrl = (picture: {
  media_type: ProfilePictureMediaType;
  bytes: Uint8Array | number[];
}): string => {
  const mediaType = ianaMediaType(picture.media_type);
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
  return `data:${mediaType};base64,${btoa(binary)}`;
};
