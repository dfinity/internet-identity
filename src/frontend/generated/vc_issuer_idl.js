export const idlFactory = ({ IDL }) => {
  const Icrc21ConsentPreferences = IDL.Record({ 'language' : IDL.Text });
  const Icrc21ConsentMessageRequest = IDL.Record({
    'arg' : IDL.Vec(IDL.Nat8),
    'method' : IDL.Text,
    'preferences' : Icrc21ConsentPreferences,
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
    'GenericError' : Icrc21ErrorInfo,
    'MalformedCall' : Icrc21ErrorInfo,
    'NotSupported' : Icrc21ErrorInfo,
    'Forbidden' : Icrc21ErrorInfo,
  });
  const Icrc21ConsentMessageResponse = IDL.Variant({
    'Ok' : Icrc21ConsentInfo,
    'Err' : Icrc21Error,
  });
  const SignedIdAlias = IDL.Record({
    'credential_jws' : IDL.Text,
    'id_alias' : IDL.Principal,
    'id_dapp' : IDL.Principal,
  });
  const CredentialSpec = IDL.Record({ 'info' : IDL.Text });
  const GetCredentialRequest = IDL.Record({
    'signed_id_alias' : SignedIdAlias,
    'prepared_context' : IDL.Opt(IDL.Vec(IDL.Nat8)),
    'credential_spec' : CredentialSpec,
  });
  const IssuedCredentialData = IDL.Record({ 'vc_jws' : IDL.Text });
  const IssueCredentialError = IDL.Variant({
    'Internal' : IDL.Text,
    'SignatureNotFound' : IDL.Text,
    'InvalidIdAlias' : IDL.Text,
    'UnauthorizedSubject' : IDL.Text,
    'UnknownSubject' : IDL.Text,
  });
  const GetCredentialResponse = IDL.Variant({
    'Ok' : IssuedCredentialData,
    'Err' : IssueCredentialError,
  });
  const PrepareCredentialRequest = IDL.Record({
    'signed_id_alias' : SignedIdAlias,
    'credential_spec' : CredentialSpec,
  });
  const PreparedCredentialData = IDL.Record({
    'prepared_context' : IDL.Opt(IDL.Vec(IDL.Nat8)),
  });
  const PrepareCredentialResponse = IDL.Variant({
    'Ok' : PreparedCredentialData,
    'Err' : IssueCredentialError,
  });
  return IDL.Service({
    'add_employee' : IDL.Func([IDL.Principal], [IDL.Text], []),
    'consent_message' : IDL.Func(
        [Icrc21ConsentMessageRequest],
        [Icrc21ConsentMessageResponse],
        [],
      ),
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
  });
};
export const init = ({ IDL }) => { return []; };
