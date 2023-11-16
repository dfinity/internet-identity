export const idlFactory = ({ IDL }) => {
  const IssuerConfig = IDL.Record({
    'idp_canister_ids' : IDL.Vec(IDL.Principal),
    'ic_root_key_der' : IDL.Vec(IDL.Nat8),
  });
  const SignedIdAlias = IDL.Record({
    'credential_jws' : IDL.Text,
    'id_alias' : IDL.Principal,
    'id_dapp' : IDL.Principal,
  });
  const ArgumentValue = IDL.Variant({ 'int' : IDL.Int32, 'string' : IDL.Text });
  const CredentialSpec = IDL.Record({
    'arguments' : IDL.Opt(IDL.Vec(IDL.Tuple(IDL.Text, ArgumentValue))),
    'credential_name' : IDL.Text,
  });
  const GetCredentialRequest = IDL.Record({
    'signed_id_alias' : SignedIdAlias,
    'prepared_context' : IDL.Opt(IDL.Vec(IDL.Nat8)),
    'credential_spec' : CredentialSpec,
  });
  const IssuedCredentialData = IDL.Record({ 'vc_jws' : IDL.Text });
  const IssueCredentialError = IDL.Variant({
    'unauthorized_subject' : IDL.Text,
    'internal' : IDL.Text,
    'signature_not_found' : IDL.Text,
    'unknown_subject' : IDL.Text,
    'invalid_id_alias' : IDL.Text,
    'unsupported_credential_spec' : IDL.Text,
  });
  const GetCredentialResponse = IDL.Variant({
    'ok' : IssuedCredentialData,
    'err' : IssueCredentialError,
  });
  const PrepareCredentialRequest = IDL.Record({
    'signed_id_alias' : SignedIdAlias,
    'credential_spec' : CredentialSpec,
  });
  const PreparedCredentialData = IDL.Record({
    'prepared_context' : IDL.Opt(IDL.Vec(IDL.Nat8)),
  });
  const PrepareCredentialResponse = IDL.Variant({
    'ok' : PreparedCredentialData,
    'err' : IssueCredentialError,
  });
  const Icrc21ConsentPreferences = IDL.Record({ 'language' : IDL.Text });
  const Icrc21VcConsentMessageRequest = IDL.Record({
    'preferences' : Icrc21ConsentPreferences,
    'credential_spec' : CredentialSpec,
  });
  const Icrc21ConsentInfo = IDL.Record({
    'consent_message' : IDL.Text,
    'language' : IDL.Text,
  });
  const Icrc21ErrorInfo = IDL.Record({
    'description' : IDL.Text,
    'error_code' : IDL.Nat64,
  });
  const Icrc21Error = IDL.Variant({
    'generic_error' : Icrc21ErrorInfo,
    'forbidden' : Icrc21ErrorInfo,
    'not_supported' : Icrc21ErrorInfo,
    'malformed_call' : Icrc21ErrorInfo,
  });
  const Icrc21ConsentMessageResponse = IDL.Variant({
    'ok' : Icrc21ConsentInfo,
    'err' : Icrc21Error,
  });
  return IDL.Service({
    'add_employee' : IDL.Func([IDL.Principal], [IDL.Text], []),
    'add_graduate' : IDL.Func([IDL.Principal], [IDL.Text], []),
    'configure' : IDL.Func([IssuerConfig], [], []),
    'get_credential' : IDL.Func(
        [GetCredentialRequest],
        [GetCredentialResponse],
        ['query'],
      ),
    'prepare_credential' : IDL.Func(
        [PrepareCredentialRequest],
        [PrepareCredentialResponse],
        [],
      ),
    'vc_consent_message' : IDL.Func(
        [Icrc21VcConsentMessageRequest],
        [Icrc21ConsentMessageResponse],
        [],
      ),
  });
};
export const init = ({ IDL }) => {
  const IssuerConfig = IDL.Record({
    'idp_canister_ids' : IDL.Vec(IDL.Principal),
    'ic_root_key_der' : IDL.Vec(IDL.Nat8),
  });
  return [IDL.Opt(IssuerConfig)];
};
