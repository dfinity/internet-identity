import { IDL } from "@icp-sdk/core/candid";

// Candid interface for the sso_gated_app canister. Kept in sync with
// ../../sso_gated_app.did (the mixin-injected sign-in methods plus the
// gated resource and debug helpers).
export const idlFactory: IDL.InterfaceFactory = ({ IDL }) => {
  const AccessError = IDL.Variant({
    NotAuthenticated: IDL.Null,
    SsoRequired: IDL.Null,
  });
  const Result = IDL.Variant({ ok: IDL.Text, err: AccessError });

  const IdentityAttributesError = IDL.Variant({
    NoAttributes: IDL.Null,
    MalformedCandid: IDL.Null,
    MissingField: IDL.Text,
    FrontendOriginsNotConfigured: IDL.Null,
    FrontendOriginMismatch: IDL.Record({
      expected: IDL.Vec(IDL.Text),
      got: IDL.Text,
    }),
    Stale: IDL.Record({ ageNs: IDL.Nat }),
    UnknownNonce: IDL.Null,
    AmbiguousAttribute: IDL.Record({
      field: IDL.Text,
      sources: IDL.Vec(IDL.Text),
    }),
    UntrustedSsoSource: IDL.Record({ domain: IDL.Text }),
    MixedSsoSources: IDL.Record({
      ssoKeys: IDL.Vec(IDL.Text),
      otherKeys: IDL.Vec(IDL.Text),
    }),
  });
  const FinishResult = IDL.Variant({
    ok: IDL.Null,
    err: IdentityAttributesError,
  });

  const Session = IDL.Record({
    name: IDL.Opt(IDL.Text),
    email: IDL.Opt(IDL.Text),
    sso: IDL.Opt(IDL.Text),
    verifiedAtNs: IDL.Nat,
  });

  return IDL.Service({
    _internet_identity_sign_in_start: IDL.Func([], [IDL.Vec(IDL.Nat8)], []),
    _internet_identity_sign_in_finish: IDL.Func([], [FinishResult], []),
    getProtectedResource: IDL.Func([], [Result], ["query"]),
    sessionInfo: IDL.Func([], [IDL.Opt(Session)], ["query"]),
    whoami: IDL.Func([], [IDL.Principal], ["query"]),
  });
};
