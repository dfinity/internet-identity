export default ({ IDL }) => {
  const InternetIdentityInit = IDL.Record({
    'assigned_user_number_range' : IDL.Tuple(IDL.Nat64, IDL.Nat64),
  });
  const UserNumber = IDL.Nat64;
  const PublicKey = IDL.Vec(IDL.Nat8);
  const DeviceKey = PublicKey;
  const CredentialId = IDL.Vec(IDL.Nat8);
  const DeviceData = IDL.Record({
    'alias' : IDL.Text,
    'pubkey' : DeviceKey,
    'credential_id' : IDL.Opt(CredentialId),
  });
  const FrontendHostname = IDL.Text;
  const SessionKey = PublicKey;
  const Timestamp = IDL.Nat64;
  const Delegation = IDL.Record({
    'pubkey' : PublicKey,
    'targets' : IDL.Opt(IDL.Vec(IDL.Principal)),
    'expiration' : Timestamp,
  });
  const SignedDelegation = IDL.Record({
    'signature' : IDL.Vec(IDL.Nat8),
    'delegation' : Delegation,
  });
  const GetDelegationResponse = IDL.Variant({
    'no_such_delegation' : IDL.Null,
    'signed_delegation' : SignedDelegation,
  });
  const HeaderField = IDL.Tuple(IDL.Text, IDL.Text);
  const HttpRequest = IDL.Record({
    'url' : IDL.Text,
    'method' : IDL.Text,
    'body' : IDL.Vec(IDL.Nat8),
    'headers' : IDL.Vec(HeaderField),
  });
  const Token = IDL.Record({});
  const StreamingCallbackHttpResponse = IDL.Record({
    'token' : IDL.Opt(Token),
    'body' : IDL.Vec(IDL.Nat8),
  });
  const StreamingStrategy = IDL.Variant({
    'Callback' : IDL.Record({
      'token' : Token,
      'callback' : IDL.Func(
          [Token],
          [StreamingCallbackHttpResponse],
          ['query'],
        ),
    }),
  });
  const HttpResponse = IDL.Record({
    'body' : IDL.Vec(IDL.Nat8),
    'headers' : IDL.Vec(HeaderField),
    'streaming_strategy' : IDL.Opt(StreamingStrategy),
    'status_code' : IDL.Nat16,
  });
  const UserKey = PublicKey;
  const InternetIdentityStats = IDL.Record({
    'users_registered' : IDL.Nat64,
    'assigned_user_number_range' : IDL.Tuple(IDL.Nat64, IDL.Nat64),
  });
  return IDL.Service({
    'add' : IDL.Func([UserNumber, DeviceData], [], []),
    'get_delegation' : IDL.Func(
        [UserNumber, FrontendHostname, SessionKey, Timestamp],
        [GetDelegationResponse],
        ['query'],
      ),
    'http_request' : IDL.Func([HttpRequest], [HttpResponse], ['query']),
    'lookup' : IDL.Func([UserNumber], [IDL.Vec(DeviceData)], ['query']),
    'prepare_delegation' : IDL.Func(
        [UserNumber, FrontendHostname, SessionKey],
        [UserKey, Timestamp],
        [],
      ),
    'register' : IDL.Func([DeviceData], [UserNumber], []),
    'remove' : IDL.Func([UserNumber, DeviceKey], [], []),
    'stats' : IDL.Func([], [InternetIdentityStats], ['query']),
  });
};
export const init = ({ IDL }) => {
  const InternetIdentityInit = IDL.Record({
    'assigned_user_number_range' : IDL.Tuple(IDL.Nat64, IDL.Nat64),
  });
  return [IDL.Opt(InternetIdentityInit)];
};
