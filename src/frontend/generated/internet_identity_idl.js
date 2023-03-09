export const idlFactory = ({ IDL }) => {
  const ArchiveConfig = IDL.Record({
    'polling_interval_ns' : IDL.Nat64,
    'entries_buffer_limit' : IDL.Nat64,
    'module_hash' : IDL.Vec(IDL.Nat8),
    'entries_fetch_limit' : IDL.Nat16,
  });
  const RateLimitConfig = IDL.Record({
    'max_tokens' : IDL.Nat64,
    'time_per_token_ns' : IDL.Nat64,
  });
  const InternetIdentityInit = IDL.Record({
    'assigned_user_number_range' : IDL.Opt(IDL.Tuple(IDL.Nat64, IDL.Nat64)),
    'archive_config' : IDL.Opt(ArchiveConfig),
    'canister_creation_cycles_cost' : IDL.Opt(IDL.Nat64),
    'register_rate_limit' : IDL.Opt(RateLimitConfig),
  });
  const UserNumber = IDL.Nat64;
  const DeviceProtection = IDL.Variant({
    'unprotected' : IDL.Null,
    'protected' : IDL.Null,
  });
  const PublicKey = IDL.Vec(IDL.Nat8);
  const DeviceKey = PublicKey;
  const KeyType = IDL.Variant({
    'platform' : IDL.Null,
    'seed_phrase' : IDL.Null,
    'cross_platform' : IDL.Null,
    'unknown' : IDL.Null,
  });
  const Purpose = IDL.Variant({
    'authentication' : IDL.Null,
    'recovery' : IDL.Null,
  });
  const CredentialId = IDL.Vec(IDL.Nat8);
  const DeviceData = IDL.Record({
    'alias' : IDL.Text,
    'origin' : IDL.Opt(IDL.Text),
    'protection' : DeviceProtection,
    'pubkey' : DeviceKey,
    'key_type' : KeyType,
    'purpose' : Purpose,
    'credential_id' : IDL.Opt(CredentialId),
  });
  const Timestamp = IDL.Nat64;
  const AddTentativeDeviceResponse = IDL.Variant({
    'device_registration_mode_off' : IDL.Null,
    'another_device_tentatively_added' : IDL.Null,
    'added_tentatively' : IDL.Record({
      'verification_code' : IDL.Text,
      'device_registration_timeout' : Timestamp,
    }),
  });
  const ChallengeKey = IDL.Text;
  const Challenge = IDL.Record({
    'png_base64' : IDL.Text,
    'challenge_key' : ChallengeKey,
  });
  const DeployArchiveResult = IDL.Variant({
    'creation_in_progress' : IDL.Null,
    'success' : IDL.Principal,
    'failed' : IDL.Text,
  });
  const BufferedArchiveEntry = IDL.Record({
    'sequence_number' : IDL.Nat64,
    'entry' : IDL.Vec(IDL.Nat8),
    'anchor_number' : UserNumber,
    'timestamp' : Timestamp,
  });
  const WebAuthnCredential = IDL.Record({
    'pubkey' : PublicKey,
    'credential_id' : CredentialId,
  });
  const AnchorCredentials = IDL.Record({
    'recovery_phrases' : IDL.Vec(PublicKey),
    'credentials' : IDL.Vec(WebAuthnCredential),
    'recovery_credentials' : IDL.Vec(WebAuthnCredential),
  });
  const DeviceWithUsage = IDL.Record({
    'alias' : IDL.Text,
    'last_usage' : IDL.Opt(Timestamp),
    'origin' : IDL.Opt(IDL.Text),
    'protection' : DeviceProtection,
    'pubkey' : DeviceKey,
    'key_type' : KeyType,
    'purpose' : Purpose,
    'credential_id' : IDL.Opt(CredentialId),
  });
  const DeviceRegistrationInfo = IDL.Record({
    'tentative_device' : IDL.Opt(DeviceData),
    'expiration' : Timestamp,
  });
  const IdentityAnchorInfo = IDL.Record({
    'devices' : IDL.Vec(DeviceWithUsage),
    'device_registration' : IDL.Opt(DeviceRegistrationInfo),
  });
  const FrontendHostname = IDL.Text;
  const SessionKey = PublicKey;
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
    'upgrade' : IDL.Opt(IDL.Bool),
    'streaming_strategy' : IDL.Opt(StreamingStrategy),
    'status_code' : IDL.Nat16,
  });
  const UserKey = PublicKey;
  const ChallengeResult = IDL.Record({
    'key' : ChallengeKey,
    'chars' : IDL.Text,
  });
  const RegisterResponse = IDL.Variant({
    'bad_challenge' : IDL.Null,
    'canister_full' : IDL.Null,
    'registered' : IDL.Record({ 'user_number' : UserNumber }),
  });
  const ArchiveInfo = IDL.Record({
    'archive_config' : IDL.Opt(ArchiveConfig),
    'archive_canister' : IDL.Opt(IDL.Principal),
  });
  const ActiveAnchorCounter = IDL.Record({
    'counter' : IDL.Nat64,
    'start_timestamp' : Timestamp,
  });
  const CompletedActiveAnchorStats = IDL.Record({
    'monthly_active_anchors' : IDL.Opt(ActiveAnchorCounter),
    'daily_active_anchors' : IDL.Opt(ActiveAnchorCounter),
  });
  const OngoingActiveAnchorStats = IDL.Record({
    'monthly_active_anchors' : IDL.Vec(ActiveAnchorCounter),
    'daily_active_anchors' : ActiveAnchorCounter,
  });
  const ActiveAnchorStatistics = IDL.Record({
    'completed' : CompletedActiveAnchorStats,
    'ongoing' : OngoingActiveAnchorStats,
  });
  const InternetIdentityStats = IDL.Record({
    'storage_layout_version' : IDL.Nat8,
    'users_registered' : IDL.Nat64,
    'assigned_user_number_range' : IDL.Tuple(IDL.Nat64, IDL.Nat64),
    'archive_info' : ArchiveInfo,
    'canister_creation_cycles_cost' : IDL.Nat64,
    'active_anchor_stats' : IDL.Opt(ActiveAnchorStatistics),
  });
  const VerifyTentativeDeviceResponse = IDL.Variant({
    'device_registration_mode_off' : IDL.Null,
    'verified' : IDL.Null,
    'wrong_code' : IDL.Record({ 'retries_left' : IDL.Nat8 }),
    'no_device_to_verify' : IDL.Null,
  });
  return IDL.Service({
    'acknowledge_entries' : IDL.Func([IDL.Nat64], [], []),
    'add' : IDL.Func([UserNumber, DeviceData], [], []),
    'add_tentative_device' : IDL.Func(
        [UserNumber, DeviceData],
        [AddTentativeDeviceResponse],
        [],
      ),
    'create_challenge' : IDL.Func([], [Challenge], []),
    'deploy_archive' : IDL.Func([IDL.Vec(IDL.Nat8)], [DeployArchiveResult], []),
    'enter_device_registration_mode' : IDL.Func([UserNumber], [Timestamp], []),
    'exit_device_registration_mode' : IDL.Func([UserNumber], [], []),
    'fetch_entries' : IDL.Func([], [IDL.Vec(BufferedArchiveEntry)], []),
    'get_anchor_credentials' : IDL.Func(
        [UserNumber],
        [AnchorCredentials],
        ['query'],
      ),
    'get_anchor_info' : IDL.Func([UserNumber], [IdentityAnchorInfo], []),
    'get_delegation' : IDL.Func(
        [UserNumber, FrontendHostname, SessionKey, Timestamp],
        [GetDelegationResponse],
        ['query'],
      ),
    'get_principal' : IDL.Func(
        [UserNumber, FrontendHostname],
        [IDL.Principal],
        ['query'],
      ),
    'http_request' : IDL.Func([HttpRequest], [HttpResponse], ['query']),
    'http_request_update' : IDL.Func([HttpRequest], [HttpResponse], []),
    'init_salt' : IDL.Func([], [], []),
    'lookup' : IDL.Func([UserNumber], [IDL.Vec(DeviceData)], ['query']),
    'prepare_delegation' : IDL.Func(
        [UserNumber, FrontendHostname, SessionKey, IDL.Opt(IDL.Nat64)],
        [UserKey, Timestamp],
        [],
      ),
    'register' : IDL.Func(
        [DeviceData, ChallengeResult],
        [RegisterResponse],
        [],
      ),
    'remove' : IDL.Func([UserNumber, DeviceKey], [], []),
    'replace' : IDL.Func([UserNumber, DeviceKey, DeviceData], [], []),
    'stats' : IDL.Func([], [InternetIdentityStats], ['query']),
    'update' : IDL.Func([UserNumber, DeviceKey, DeviceData], [], []),
    'verify_tentative_device' : IDL.Func(
        [UserNumber, IDL.Text],
        [VerifyTentativeDeviceResponse],
        [],
      ),
  });
};
export const init = ({ IDL }) => {
  const ArchiveConfig = IDL.Record({
    'polling_interval_ns' : IDL.Nat64,
    'entries_buffer_limit' : IDL.Nat64,
    'module_hash' : IDL.Vec(IDL.Nat8),
    'entries_fetch_limit' : IDL.Nat16,
  });
  const RateLimitConfig = IDL.Record({
    'max_tokens' : IDL.Nat64,
    'time_per_token_ns' : IDL.Nat64,
  });
  const InternetIdentityInit = IDL.Record({
    'assigned_user_number_range' : IDL.Opt(IDL.Tuple(IDL.Nat64, IDL.Nat64)),
    'archive_config' : IDL.Opt(ArchiveConfig),
    'canister_creation_cycles_cost' : IDL.Opt(IDL.Nat64),
    'register_rate_limit' : IDL.Opt(RateLimitConfig),
  });
  return [IDL.Opt(InternetIdentityInit)];
};
