import { Principal } from "@icp-sdk/core/principal";
import { z } from "zod";

export const PROMPT_PARAM = "prompt";
export const HINT_PARAM = "hint";

/** Survives the round trip an interactive flow may take through an IdP. */
const STORAGE_KEY = "ii-authorize-prompt";

export type AuthorizationPrompt = "none" | "login";

export interface PromptContext {
  prompt?: AuthorizationPrompt;
  hint?: string;
}

const isPrincipal = (value: string): boolean => {
  try {
    Principal.fromText(value);
    return true;
  } catch {
    return false;
  }
};

// `prompt` and `hint` are preferences, never credentials, so an unreadable value
// degrades to an interactive sign-in rather than failing the request.
const PromptContextSchema = z.object({
  prompt: z.enum(["none", "login"]).optional().catch(undefined),
  hint: z
    .string()
    .refine(isPrincipal)
    .transform((value) => Principal.fromText(value).toText())
    .optional()
    .catch(undefined),
});

export const readPromptParams = (url: URL): PromptContext => {
  const parsed = PromptContextSchema.safeParse({
    prompt: url.searchParams.get(PROMPT_PARAM) ?? undefined,
    hint: url.searchParams.get(HINT_PARAM) ?? undefined,
  });
  return parsed.success ? parsed.data : {};
};

export const resolvePromptParams = (
  url: URL,
  isResuming: boolean,
): PromptContext => {
  if (isResuming) {
    const stored = sessionStorage.getItem(STORAGE_KEY);
    if (stored === null) {
      return {};
    }
    try {
      const parsed = PromptContextSchema.safeParse(JSON.parse(stored));
      return parsed.success ? parsed.data : {};
    } catch {
      return {};
    }
  }

  const context = readPromptParams(url);
  if (context.prompt === undefined && context.hint === undefined) {
    sessionStorage.removeItem(STORAGE_KEY);
  } else {
    sessionStorage.setItem(STORAGE_KEY, JSON.stringify(context));
  }
  return context;
};

/** Keeps the address bar free of values the flow has already consumed. */
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
  window.history.replaceState(null, "", url.toString());
};
