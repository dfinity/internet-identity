export const idlFactory = ({ IDL }) => {
  const MetadataMap = IDL.Rec();
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
    'max_num_latest_delegation_origins' : IDL.Opt(IDL.Nat64),
    'assigned_user_number_range' : IDL.Opt(IDL.Tuple(IDL.Nat64, IDL.Nat64)),
    'archive_config' : IDL.Opt(ArchiveConfig),
    'canister_creation_cycles_cost' : IDL.Opt(IDL.Nat64),
    'register_rate_limit' : IDL.Opt(RateLimitConfig),
  });
  const IdentityNumber = IDL.Nat64;
  const Timestamp = IDL.Nat64;
  MetadataMap.fill(
    IDL.Vec(
      IDL.Tuple(
        IDL.Text,
        IDL.Variant({
          'map' : MetadataMap,
          'string' : IDL.Text,
          'bytes' : IDL.Vec(IDL.Nat8),
        }),
      )
    )
  );
  const WebAuthnDirectSigMode = IDL.Variant({
    'optional' : IDL.Null,
    'mandatory' : IDL.Null,
  });
  const PublicKey = IDL.Vec(IDL.Nat8);
  const CredentialId = IDL.Vec(IDL.Nat8);
  const Auth = IDL.Variant({
    'webauthn_key' : IDL.Record({
      'direct_signature' : WebAuthnDirectSigMode,
      'pubkey' : PublicKey,
      'credential_id' : CredentialId,
    }),
    'generic_key' : IDL.Record({ 'pubkey' : PublicKey }),
  });
  const AuthProtection = IDL.Variant({
    'unprotected' : IDL.Null,
    'protected' : IDL.Null,
  });
  const AuthRecord = IDL.Record({
    'last_usage' : IDL.Opt(Timestamp),
    'metadata' : MetadataMap,
    'auth' : Auth,
    'protection' : AuthProtection,
  });
  const result = IDL.Variant({ 'ok' : IDL.Null });
  const AddTentativeAuthResponse = IDL.Variant({
    'another_auth_tentatively_added' : IDL.Null,
    'added_tentatively' : IDL.Record({
      'verification_code' : IDL.Text,
      'device_registration_timeout' : Timestamp,
    }),
    'auth_registration_mode_off' : IDL.Null,
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
    'anchor_number' : IdentityNumber,
    'timestamp' : Timestamp,
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
  const IdentityAuthInfo = IDL.Record({
    'auth' : IDL.Vec(Auth),
    'recovery' : IDL.Vec(Auth),
  });
  const AuthRegistrationInfo = IDL.Record({
    'expiration' : Timestamp,
    'tentative_auth' : IDL.Opt(AuthRecord),
  });
  const IdentityInfo = IDL.Record({
    'auth_records' : IDL.Vec(AuthRecord),
    'auth_registration' : IDL.Opt(AuthRegistrationInfo),
    'recovery_records' : IDL.Vec(AuthRecord),
  });
  const ChallengeResult = IDL.Record({
    'key' : ChallengeKey,
    'chars' : IDL.Text,
  });
  const RegisterResponse = IDL.Variant({
    'bad_challenge' : IDL.Null,
    'canister_full' : IDL.Null,
    'registered' : IDL.Record({ 'identity_number' : IdentityNumber }),
  });
  const DomainActiveAnchorCounter = IDL.Record({
    'start_timestamp' : Timestamp,
    'internetcomputer_org_counter' : IDL.Nat64,
    'ic0_app_counter' : IDL.Nat64,
    'both_ii_domains_counter' : IDL.Nat64,
  });
  const DomainCompletedActiveAnchorStats = IDL.Record({
    'monthly_active_anchors' : IDL.Opt(DomainActiveAnchorCounter),
    'daily_active_anchors' : IDL.Opt(DomainActiveAnchorCounter),
  });
  const DomainOngoingActiveAnchorStats = IDL.Record({
    'monthly_active_anchors' : IDL.Vec(DomainActiveAnchorCounter),
    'daily_active_anchors' : DomainActiveAnchorCounter,
  });
  const DomainActiveAnchorStatistics = IDL.Record({
    'completed' : DomainCompletedActiveAnchorStats,
    'ongoing' : DomainOngoingActiveAnchorStats,
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
    'domain_active_anchor_stats' : IDL.Opt(DomainActiveAnchorStatistics),
    'max_num_latest_delegation_origins' : IDL.Nat64,
    'assigned_user_number_range' : IDL.Tuple(IDL.Nat64, IDL.Nat64),
    'latest_delegation_origins' : IDL.Vec(FrontendHostname),
    'archive_info' : ArchiveInfo,
    'canister_creation_cycles_cost' : IDL.Nat64,
    'active_anchor_stats' : IDL.Opt(ActiveAnchorStatistics),
  });
  const AuthSettings = IDL.Record({
    'direct_signature' : WebAuthnDirectSigMode,
    'protection' : AuthProtection,
  });
  const VerifyTentativeAuthResponse = IDL.Variant({
    'device_registration_mode_off' : IDL.Null,
    'verified' : IDL.Null,
    'wrong_code' : IDL.Record({ 'retries_left' : IDL.Nat8 }),
    'no_device_to_verify' : IDL.Null,
  });
  return IDL.Service({
    'acknowledge_entries' : IDL.Func([IDL.Nat64], [], []),
    'add_auth' : IDL.Func(
        [IdentityNumber, AuthRecord, IDL.Opt(IDL.Vec(IDL.Nat8))],
        [IDL.Opt(result)],
        [],
      ),
    'add_recovery' : IDL.Func(
        [IdentityNumber, AuthRecord, IDL.Opt(IDL.Vec(IDL.Nat8))],
        [IDL.Opt(result)],
        [],
      ),
    'add_tentative_auth' : IDL.Func(
        [IdentityNumber, Auth, MetadataMap],
        [IDL.Opt(IDL.Variant({ 'ok' : AddTentativeAuthResponse }))],
        [],
      ),
    'create_captcha' : IDL.Func(
        [],
        [IDL.Opt(IDL.Variant({ 'ok' : Challenge }))],
        [],
      ),
    'deploy_archive' : IDL.Func([IDL.Vec(IDL.Nat8)], [DeployArchiveResult], []),
    'enter_auth_registration_mode' : IDL.Func(
        [IdentityNumber],
        [IDL.Opt(IDL.Variant({ 'ok' : Timestamp }))],
        [],
      ),
    'exit_auth_registration_mode' : IDL.Func(
        [IdentityNumber],
        [IDL.Opt(result)],
        [],
      ),
    'fetch_entries' : IDL.Func([], [IDL.Vec(BufferedArchiveEntry)], []),
    'get_delegation' : IDL.Func(
        [IdentityNumber, FrontendHostname, SessionKey, Timestamp],
        [GetDelegationResponse],
        ['query'],
      ),
    'get_principal' : IDL.Func(
        [IdentityNumber, FrontendHostname],
        [IDL.Principal],
        ['query'],
      ),
    'http_request' : IDL.Func([HttpRequest], [HttpResponse], ['query']),
    'http_request_update' : IDL.Func([HttpRequest], [HttpResponse], []),
    'identity_auth_info' : IDL.Func(
        [IdentityNumber],
        [IDL.Opt(IDL.Variant({ 'ok' : IdentityAuthInfo }))],
        ['query'],
      ),
    'identity_info' : IDL.Func(
        [IdentityNumber],
        [IDL.Opt(IDL.Variant({ 'ok' : IdentityInfo }))],
        [],
      ),
    'init_salt' : IDL.Func([], [], []),
    'prepare_delegation' : IDL.Func(
        [IdentityNumber, FrontendHostname, SessionKey, IDL.Opt(IDL.Nat64)],
        [PublicKey, Timestamp],
        [],
      ),
    'register_identity' : IDL.Func(
        [AuthRecord, ChallengeResult, IDL.Opt(IDL.Principal)],
        [IDL.Opt(RegisterResponse)],
        [],
      ),
    'remove_auth' : IDL.Func(
        [IdentityNumber, PublicKey, IDL.Opt(IDL.Vec(IDL.Nat8))],
        [IDL.Opt(result)],
        [],
      ),
    'replace_auth' : IDL.Func(
        [IdentityNumber, PublicKey, AuthRecord, IDL.Opt(IDL.Vec(IDL.Nat8))],
        [IDL.Opt(result)],
        [],
      ),
    'stats' : IDL.Func([], [InternetIdentityStats], ['query']),
    'update_auth_metadata' : IDL.Func(
        [IdentityNumber, PublicKey, MetadataMap, IDL.Opt(Timestamp)],
        [IDL.Opt(result)],
        [],
      ),
    'update_auth_settings' : IDL.Func(
        [IdentityNumber, PublicKey, AuthSettings, IDL.Opt(IDL.Vec(IDL.Nat8))],
        [IDL.Opt(result)],
        [],
      ),
    'verify_tentative_auth' : IDL.Func(
        [IdentityNumber, IDL.Text, IDL.Opt(IDL.Vec(IDL.Nat8))],
        [IDL.Opt(IDL.Variant({ 'ok' : VerifyTentativeAuthResponse }))],
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
    'max_num_latest_delegation_origins' : IDL.Opt(IDL.Nat64),
    'assigned_user_number_range' : IDL.Opt(IDL.Tuple(IDL.Nat64, IDL.Nat64)),
    'archive_config' : IDL.Opt(ArchiveConfig),
    'canister_creation_cycles_cost' : IDL.Opt(IDL.Nat64),
    'register_rate_limit' : IDL.Opt(RateLimitConfig),
  });
  return [IDL.Opt(InternetIdentityInit)];
};
