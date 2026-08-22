import { z } from "zod";
import { Principal } from "@icp-sdk/core/principal";
import type { AuthorizationPromptContext } from "$lib/stores/authorization.store";

const PROMPT_PARAM = "prompt";
const HINT_PARAM = "hint";

// Carries the params across an Internet Identity-internal redirect to an
// identity provider, which unloads the page and returns to a bare
// `/authorize?flow=openid-resume`. Without this the resumed load would read no
// `prompt`, treat a `prompt=login` request as if it had asked for nothing, and
// answer from the cached delegation the user was just made to sign in past.
const STORAGE_KEY = "ii-authorize-prompt";

const isPrincipal = (value: string): boolean => {
  try {
    Principal.fromText(value);
    return true;
  } catch {
    return false;
  }
};

/**
 * What an app may ask for about the sign-in itself.
 *
 * Used for both the URL and the copy kept across an identity provider
 * round-trip, so neither path can end up validating more loosely than the other.
 *
 * Each field falls back to `undefined` instead of failing the parse. An
 * unrecognised `prompt` (OpenID Connect's `consent` and `select_account`, which
 * Internet Identity does not implement) or a `hint` that is not a principal
 * earns default behaviour, not a rejected sign-in.
 */
const PromptContextSchema = z.object({
  prompt: z.enum(["none", "login"]).optional().catch(undefined),
  hint: z
    .string()
    .refine(isPrincipal)
    // Normalised, because it is compared against a stored `Principal.toText()`.
    .transform((value) => Principal.fromText(value).toText())
    .optional()
    .catch(undefined),
});

const parse = (input: unknown): AuthorizationPromptContext => {
  const result = PromptContextSchema.safeParse(input);
  return result.success ? result.data : {};
};

const isEmpty = (context: AuthorizationPromptContext): boolean =>
  context.prompt === undefined && context.hint === undefined;

export const readPromptParams = (url: URL): AuthorizationPromptContext =>
  parse({
    prompt: url.searchParams.get(PROMPT_PARAM),
    hint: url.searchParams.get(HINT_PARAM),
  });

const readStored = (): AuthorizationPromptContext | undefined => {
  const json = sessionStorage.getItem(STORAGE_KEY);
  if (json === null) {
    return undefined;
  }
  try {
    return parse(JSON.parse(json));
  } catch {
    return undefined;
  }
};

/**
 * Resolves what the app asked for on this load, and keeps it available across an
 * identity provider round-trip.
 *
 * Any load that is not resuming such a round-trip rewrites or clears the stored
 * copy, so a value cannot outlive the flow that set it and be picked up by an
 * unrelated later one.
 */
export const resolvePromptParams = (
  url: URL,
  isResuming: boolean,
): AuthorizationPromptContext => {
  const fromUrl = readPromptParams(url);
  if (isResuming) {
    return readStored() ?? fromUrl;
  }
  if (isEmpty(fromUrl)) {
    sessionStorage.removeItem(STORAGE_KEY);
  } else {
    sessionStorage.setItem(STORAGE_KEY, JSON.stringify(fromUrl));
  }
  return fromUrl;
};

/** Drops the params from the address bar, so a principal does not linger in
 *  history and a copied URL cannot replay a silent sign-in. Leaves every other
 *  param, and the fragment, untouched. */
export const stripPromptParams = (): void => {
  const url = new URL(window.location.href);
  if (
    !url.searchParams.has(PROMPT_PARAM) &&
    !url.searchParams.has(HINT_PARAM)
  ) {
    return;
  }
  url.searchParams.delete(PROMPT_PARAM);
  url.searchParams.delete(HINT_PARAM);
  window.history.replaceState(null, "", url);
};
