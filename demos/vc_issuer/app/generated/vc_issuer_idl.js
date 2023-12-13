export const idlFactory = ({ IDL }) => {
  const IssuerConfig = IDL.Record({
    'idp_canister_ids' : IDL.Vec(IDL.Principal),
    'ic_root_key_der' : IDL.Vec(IDL.Nat8),
  });
  const SignedIdAlias = IDL.Record({ 'credential_jws' : IDL.Text });
  const ArgumentValue = IDL.Variant({ 'Int' : IDL.Int32, 'String' : IDL.Text });
  const CredentialSpec = IDL.Record({
    'arguments' : IDL.Opt(IDL.Vec(IDL.Tuple(IDL.Text, ArgumentValue))),
    'credential_type' : IDL.Text,
  });
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
    'UnsupportedCredentialSpec' : IDL.Text,
  });
  const GetCredentialResponse = IDL.Variant({
    'Ok' : IssuedCredentialData,
    'Err' : IssueCredentialError,
  });
  const HeaderField = IDL.Tuple(IDL.Text, IDL.Text);
  const HttpRequest = IDL.Record({
    'url' : IDL.Text,
    'method' : IDL.Text,
    'body' : IDL.Vec(IDL.Nat8),
    'headers' : IDL.Vec(HeaderField),
    'certificate_version' : IDL.Opt(IDL.Nat16),
  });
  const HttpResponse = IDL.Record({
    'body' : IDL.Vec(IDL.Nat8),
    'headers' : IDL.Vec(HeaderField),
    'status_code' : IDL.Nat16,
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
  const Icrc21ConsentPreferences = IDL.Record({ 'language' : IDL.Text });
  const Icrc21VcConsentMessageRequest = IDL.Record({
    'preferences' : Icrc21ConsentPreferences,
    'credential_spec' : CredentialSpec,
  });
  const Icrc21ConsentInfo = IDL.Record({
    'consent_message' : IDL.Text,
    'language' : IDL.Text,
  });
  const Icrc21ErrorInfo = IDL.Record({ 'description' : IDL.Text });
  const Icrc21Error = IDL.Variant({
    'GenericError' : IDL.Record({
      'description' : IDL.Text,
      'error_code' : IDL.Nat,
    }),
    'UnsupportedCanisterCall' : Icrc21ErrorInfo,
    'ConsentMessageUnavailable' : Icrc21ErrorInfo,
  });
  const Icrc21ConsentMessageResponse = IDL.Variant({
    'Ok' : Icrc21ConsentInfo,
    'Err' : Icrc21Error,
  });
  return IDL.Service({
    'add_adult' : IDL.Func([IDL.Principal], [IDL.Text], []),
    'add_employee' : IDL.Func([IDL.Principal], [IDL.Text], []),
    'add_graduate' : IDL.Func([IDL.Principal], [IDL.Text], []),
    'configure' : IDL.Func([IssuerConfig], [], []),
    'get_credential' : IDL.Func(
        [GetCredentialRequest],
        [GetCredentialResponse],
        ['query'],
      ),
    'http_request' : IDL.Func([HttpRequest], [HttpResponse], ['query']),
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
