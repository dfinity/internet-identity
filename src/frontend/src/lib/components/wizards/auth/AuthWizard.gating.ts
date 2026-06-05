export type AuthMode = "signin" | "signup" | "both";
export type OpenIdContinueResultType = "signIn" | "signUp";

// Returns the handler when the OpenID "already linked" dialog should be
// dispatched (result is an existing sign-in while the picker is in sign-up
// mode), and `undefined` otherwise — letting callers narrow the handler in
// a single check.
export const resolveOpenIdAlreadyLinkedDispatcher = <H>(
  resultType: OpenIdContinueResultType,
  mode: AuthMode,
  handler: H | undefined,
): H | undefined =>
  resultType === "signIn" && mode === "signup" ? handler : undefined;

// Returns the handler when the OpenID "not connected" dialog should be
// dispatched (result is a fresh sign-up while the picker is in sign-in
// mode), and `undefined` otherwise.
export const resolveOpenIdNotConnectedDispatcher = <H>(
  resultType: OpenIdContinueResultType,
  mode: AuthMode,
  handler: H | undefined,
): H | undefined =>
  resultType === "signUp" && mode === "signin" ? handler : undefined;
