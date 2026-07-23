export const idlFactory = ({ IDL }) => {
  const MetadataMap = IDL.Rec();
  const MetadataMapV2 = IDL.Rec();
  const DohConfig = IDL.Record({
    'max_cache_age_secs' : IDL.Opt(IDL.Nat64),
    'allowed_domains' : IDL.Vec(IDL.Text),
  });
  const SsoCredentialMigrationEntry = IDL.Record({
    'name' : IDL.Opt(IDL.Text),
    'issuer' : IDL.Text,
    'discovery_domain' : IDL.Text,
    'client_id' : IDL.Text,
  });
  const DnssecRootAnchor = IDL.Record({
    'algorithm' : IDL.Nat8,
    'key_tag' : IDL.Nat16,
    'digest_type' : IDL.Nat8,
    'digest' : IDL.Vec(IDL.Nat8),
  });
  const DnssecConfig = IDL.Record({
    'root_anchors' : IDL.Vec(DnssecRootAnchor),
  });
  const ArchiveConfig = IDL.Record({
    'polling_interval_ns' : IDL.Nat64,
    'entries_buffer_limit' : IDL.Nat64,
    'module_hash' : IDL.Vec(IDL.Nat8),
    'entries_fetch_limit' : IDL.Nat16,
  });
  const AnalyticsConfig = IDL.Variant({
    'Plausible' : IDL.Record({
      'domain' : IDL.Opt(IDL.Text),
      'track_localhost' : IDL.Opt(IDL.Bool),
      'hash_mode' : IDL.Opt(IDL.Bool),
      'api_host' : IDL.Opt(IDL.Text),
    }),
  });
  const OpenIdEmailVerification = IDL.Variant({
    'Google' : IDL.Null,
    'Unknown' : IDL.Null,
    'Microsoft' : IDL.Null,
  });
  const OpenIdConfig = IDL.Record({
    'auth_uri' : IDL.Text,
    'jwks_uri' : IDL.Text,
    'logo' : IDL.Text,
    'name' : IDL.Text,
    'fedcm_uri' : IDL.Opt(IDL.Text),
    'email_verification' : IDL.Opt(OpenIdEmailVerification),
    'issuer' : IDL.Text,
    'auth_scope' : IDL.Vec(IDL.Text),
    'seed_jwks' : IDL.Opt(IDL.Vec(IDL.Vec(IDL.Tuple(IDL.Text, IDL.Text)))),
    'client_id' : IDL.Text,
  });
  const CaptchaConfig = IDL.Record({
    'max_unsolved_captchas' : IDL.Nat64,
    'captcha_trigger' : IDL.Variant({
      'Dynamic' : IDL.Record({
        'reference_rate_sampling_interval_s' : IDL.Nat64,
        'threshold_pct' : IDL.Nat16,
        'current_rate_sampling_interval_s' : IDL.Nat64,
      }),
      'Static' : IDL.Variant({
        'CaptchaDisabled' : IDL.Null,
        'CaptchaEnabled' : IDL.Null,
      }),
    }),
  });
  const DummyAuthConfig = IDL.Record({ 'prompt_for_index' : IDL.Bool });
  const RateLimitConfig = IDL.Record({
    'max_tokens' : IDL.Nat64,
    'time_per_token_ns' : IDL.Nat64,
  });
  const InternetIdentityInit = IDL.Record({
    'doh_config' : IDL.Opt(IDL.Opt(DohConfig)),
    'sso_credential_migration' : IDL.Opt(IDL.Vec(SsoCredentialMigrationEntry)),
    'is_production' : IDL.Opt(IDL.Bool),
    'backend_canister_id' : IDL.Opt(IDL.Principal),
    'enable_dapps_explorer' : IDL.Opt(IDL.Bool),
    'assigned_user_number_range' : IDL.Opt(IDL.Tuple(IDL.Nat64, IDL.Nat64)),
    'new_flow_origins' : IDL.Opt(IDL.Vec(IDL.Text)),
    'dnssec_config' : IDL.Opt(IDL.Opt(DnssecConfig)),
    'archive_config' : IDL.Opt(ArchiveConfig),
    'canister_creation_cycles_cost' : IDL.Opt(IDL.Nat64),
    'analytics_config' : IDL.Opt(IDL.Opt(AnalyticsConfig)),
    'enable_dnssec_email_recovery' : IDL.Opt(IDL.Bool),
    'related_origins' : IDL.Opt(IDL.Vec(IDL.Text)),
    'openid_configs' : IDL.Opt(IDL.Vec(OpenIdConfig)),
    'backend_origin' : IDL.Opt(IDL.Text),
    'captcha_config' : IDL.Opt(CaptchaConfig),
    'dummy_auth' : IDL.Opt(IDL.Opt(DummyAuthConfig)),
    'sso_allow_insecure_discovery' : IDL.Opt(IDL.Bool),
    'register_rate_limit' : IDL.Opt(RateLimitConfig),
  });
  const UserNumber = IDL.Nat64;
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
    'browser_storage_key' : IDL.Null,
  });
  const Aaguid = IDL.Vec(IDL.Nat8);
  const Purpose = IDL.Variant({
    'authentication' : IDL.Null,
    'recovery' : IDL.Null,
  });
  const CredentialId = IDL.Vec(IDL.Nat8);
  const DeviceData = IDL.Record({
    'alias' : IDL.Text,
    'metadata' : IDL.Opt(MetadataMap),
    'origin' : IDL.Opt(IDL.Text),
    'protection' : DeviceProtection,
    'pubkey' : DeviceKey,
    'key_type' : KeyType,
    'aaguid' : IDL.Opt(Aaguid),
    'purpose' : Purpose,
    'credential_id' : IDL.Opt(CredentialId),
  });
  const Timestamp = IDL.Nat64;
  const AddTentativeDeviceResponse = IDL.Variant({
    'device_registration_mode_off' : IDL.Null,
    'another_device_tentatively_added' : IDL.Null,
    'passkey_with_this_public_key_is_already_used' : IDL.Null,
    'added_tentatively' : IDL.Record({
      'verification_code' : IDL.Text,
      'device_registration_timeout' : Timestamp,
    }),
  });
  const IdentityNumber = IDL.Nat64;
  const AuthnMethodProtection = IDL.Variant({
    'Protected' : IDL.Null,
    'Unprotected' : IDL.Null,
  });
  const AuthnMethodPurpose = IDL.Variant({
    'Recovery' : IDL.Null,
    'Authentication' : IDL.Null,
  });
  const AuthnMethodSecuritySettings = IDL.Record({
    'protection' : AuthnMethodProtection,
    'purpose' : AuthnMethodPurpose,
  });
  MetadataMapV2.fill(
    IDL.Vec(
      IDL.Tuple(
        IDL.Text,
        IDL.Variant({
          'Map' : MetadataMapV2,
          'String' : IDL.Text,
          'Bytes' : IDL.Vec(IDL.Nat8),
        }),
      )
    )
  );
  const PublicKeyAuthn = IDL.Record({ 'pubkey' : PublicKey });
  const WebAuthn = IDL.Record({
    'pubkey' : PublicKey,
    'aaguid' : IDL.Opt(Aaguid),
    'credential_id' : CredentialId,
  });
  const AuthnMethod = IDL.Variant({
    'PubKey' : PublicKeyAuthn,
    'WebAuthn' : WebAuthn,
  });
  const AuthnMethodData = IDL.Record({
    'security_settings' : AuthnMethodSecuritySettings,
    'metadata' : MetadataMapV2,
    'last_authentication' : IDL.Opt(Timestamp),
    'authn_method' : AuthnMethod,
  });
  const AuthnMethodAddError = IDL.Variant({ 'InvalidMetadata' : IDL.Text });
  const AuthnMethodConfirmationError = IDL.Variant({
    'InternalCanisterError' : IDL.Text,
    'RegistrationModeOff' : IDL.Null,
    'Unauthorized' : IDL.Principal,
    'NoAuthnMethodToConfirm' : IDL.Null,
    'WrongCode' : IDL.Record({ 'retries_left' : IDL.Nat8 }),
  });
  const AuthnMethodMetadataReplaceError = IDL.Variant({
    'AuthnMethodNotFound' : IDL.Null,
    'InvalidMetadata' : IDL.Text,
  });
  const AuthnMethodConfirmationCode = IDL.Record({
    'confirmation_code' : IDL.Text,
    'expiration' : Timestamp,
  });
  const AuthnMethodRegisterError = IDL.Variant({
    'PasskeyWithThisPublicKeyIsAlreadyUsed' : IDL.Null,
    'RegistrationModeOff' : IDL.Null,
    'RegistrationAlreadyInProgress' : IDL.Null,
    'NotSelfAuthenticating' : IDL.Principal,
    'InvalidMetadata' : IDL.Text,
  });
  const RegistrationId = IDL.Text;
  const AuthnMethodRegistrationModeEnterError = IDL.Variant({
    'InvalidRegistrationId' : IDL.Text,
    'InternalCanisterError' : IDL.Text,
    'AlreadyInProgress' : IDL.Null,
    'Unauthorized' : IDL.Principal,
  });
  const AuthnMethodRegistrationModeExitError = IDL.Variant({
    'PasskeyWithThisPublicKeyIsAlreadyUsed' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'RegistrationModeOff' : IDL.Null,
    'Unauthorized' : IDL.Principal,
    'InvalidMetadata' : IDL.Text,
  });
  const AuthnMethodReplaceError = IDL.Variant({
    'PasskeyWithThisPublicKeyIsAlreadyUsed' : IDL.Null,
    'AuthnMethodNotFound' : IDL.Null,
    'InvalidMetadata' : IDL.Text,
  });
  const AuthnMethodSecuritySettingsReplaceError = IDL.Variant({
    'AuthnMethodNotFound' : IDL.Null,
  });
  const AuthnMethodSessionInfo = IDL.Record({
    'name' : IDL.Opt(IDL.Text),
    'created_at' : IDL.Opt(Timestamp),
  });
  const CheckCaptchaArg = IDL.Record({ 'solution' : IDL.Text });
  const RegistrationFlowNextStep = IDL.Variant({
    'CheckCaptcha' : IDL.Record({ 'captcha_png_base64' : IDL.Text }),
    'Finish' : IDL.Null,
  });
  const IdRegNextStepResult = IDL.Record({
    'next_step' : RegistrationFlowNextStep,
  });
  const CheckCaptchaError = IDL.Variant({
    'NoRegistrationFlow' : IDL.Null,
    'UnexpectedCall' : IDL.Record({ 'next_step' : RegistrationFlowNextStep }),
    'WrongSolution' : IDL.Record({ 'new_captcha_png_base64' : IDL.Text }),
  });
  const FrontendHostname = IDL.Text;
  const AccountNumber = IDL.Nat64;
  const AccountInfo = IDL.Record({
    'name' : IDL.Opt(IDL.Text),
    'origin' : IDL.Text,
    'account_number' : IDL.Opt(AccountNumber),
    'last_used' : IDL.Opt(Timestamp),
  });
  const CreateAccountError = IDL.Variant({
    'AccountLimitReached' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
    'NameTooLong' : IDL.Null,
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
  const VerificationPath = IDL.Variant({
    'Doh' : IDL.Null,
    'Dnssec' : IDL.Null,
  });
  const EmailChallengeDiagnostics = IDL.Record({
    'created_at' : Timestamp,
    'verification_path' : VerificationPath,
    'message_id' : IDL.Opt(IDL.Text),
    'reason_code' : IDL.Text,
  });
  const EmailChallengeResolveViaDohArg = IDL.Record({ 'nonce' : IDL.Text });
  const DohFailureReason = IDL.Variant({
    'AllProvidersFailed' : IDL.Null,
    'ResponseMalformed' : IDL.Text,
    'QuorumFailed' : IDL.Record({
      'total' : IDL.Nat32,
      'agreeing' : IDL.Nat32,
    }),
  });
  const EmailChallengeError = IDL.Variant({
    'EmailVerificationFailed' : IDL.Text,
    'DkimLeafMismatch' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'NonceUnknown' : IDL.Null,
    'DohFetchFailed' : DohFailureReason,
    'NoDkimLeafExpected' : IDL.Null,
    'LimitReached' : IDL.Record({ 'limit' : IDL.Nat8 }),
    'DomainNotSupported' : IDL.Text,
    'AddressNotRegistered' : IDL.Null,
    'EmptyDkimLeafHops' : IDL.Null,
    'Unauthorized' : IDL.Principal,
    'NonceExpired' : IDL.Null,
    'AddressMismatch' : IDL.Null,
    'InvalidEmailAddress' : IDL.Text,
    'DomainNotAllowlisted' : IDL.Text,
    'SubjectNotSigned' : IDL.Null,
    'AddressAlreadyRegistered' : IDL.Null,
  });
  const UserKey = PublicKey;
  const EmailChallengeStatus = IDL.Variant({
    'Failed' : EmailChallengeError,
    'ResolvingDoh' : IDL.Null,
    'NeedDkimLeaf' : IDL.Record({ 'selector' : IDL.Text }),
    'RecoveryReady' : IDL.Record({
      'user_key' : UserKey,
      'expiration' : Timestamp,
      'anchor_number' : IdentityNumber,
    }),
    'RegistrationSucceeded' : IDL.Null,
    'Expired' : IDL.Null,
    'Pending' : IDL.Null,
  });
  const Rrsig = IDL.Record({
    'algorithm' : IDL.Nat8,
    'signature' : IDL.Vec(IDL.Nat8),
    'original_ttl' : IDL.Nat32,
    'signer_name' : IDL.Vec(IDL.Nat8),
    'labels' : IDL.Nat8,
    'inception' : IDL.Nat32,
    'expiration' : IDL.Nat32,
    'key_tag' : IDL.Nat16,
    'type_covered' : IDL.Nat16,
  });
  const SignedRRset = IDL.Record({
    'ttl' : IDL.Nat32,
    'name' : IDL.Vec(IDL.Nat8),
    'rdata' : IDL.Vec(IDL.Vec(IDL.Nat8)),
    'rrsig' : Rrsig,
    'rtype' : IDL.Nat16,
  });
  const DelegationLink = IDL.Record({
    'child_dnskey' : SignedRRset,
    'child_ds' : SignedRRset,
  });
  const DelegationChain = IDL.Record({ 'links' : IDL.Vec(DelegationLink) });
  const EmailChallengeSubmitDkimLeafArg = IDL.Record({
    'extra_chains' : IDL.Vec(DelegationChain),
    'hops' : IDL.Vec(SignedRRset),
    'nonce' : IDL.Text,
  });
  const DnsProofBundle = IDL.Record({
    'root_dnskey' : SignedRRset,
    'hops' : IDL.Vec(SignedRRset),
    'chains' : IDL.Vec(DelegationChain),
  });
  const EmailChallengeDnsInput = IDL.Record({
    'dns_proof' : IDL.Opt(DnsProofBundle),
    'address' : IDL.Text,
  });
  const EmailChallenge = IDL.Record({
    'nonce' : IDL.Text,
    'expires_at' : Timestamp,
  });
  const SessionKey = PublicKey;
  const EmailRecoveryGetDelegationArgs = IDL.Record({
    'session_key' : SessionKey,
    'expiration' : Timestamp,
    'nonce' : IDL.Text,
  });
  const Delegation = IDL.Record({
    'permissions' : IDL.Opt(IDL.Text),
    'pubkey' : PublicKey,
    'targets' : IDL.Opt(IDL.Vec(IDL.Principal)),
    'expiration' : Timestamp,
  });
  const SignedDelegation = IDL.Record({
    'signature' : IDL.Vec(IDL.Nat8),
    'delegation' : Delegation,
  });
  const BufferedArchiveEntry = IDL.Record({
    'sequence_number' : IDL.Nat64,
    'entry' : IDL.Vec(IDL.Nat8),
    'anchor_number' : UserNumber,
    'timestamp' : Timestamp,
  });
  const Permissions = IDL.Variant({ 'all' : IDL.Null, 'queries' : IDL.Null });
  const AccountDelegationError = IDL.Variant({
    'NoSuchDelegation' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
  });
  const GetAccountsError = IDL.Variant({
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
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
    'metadata' : IDL.Opt(MetadataMap),
    'origin' : IDL.Opt(IDL.Text),
    'protection' : DeviceProtection,
    'pubkey' : DeviceKey,
    'key_type' : KeyType,
    'aaguid' : IDL.Opt(Aaguid),
    'purpose' : Purpose,
    'credential_id' : IDL.Opt(CredentialId),
  });
  const Aud = IDL.Text;
  const Iss = IDL.Text;
  const Sub = IDL.Text;
  const OpenIdCredential = IDL.Record({
    'aud' : Aud,
    'iss' : Iss,
    'sub' : Sub,
    'metadata' : MetadataMapV2,
    'sso_domain' : IDL.Opt(IDL.Text),
    'sso_name' : IDL.Opt(IDL.Text),
    'last_usage_timestamp' : IDL.Opt(Timestamp),
  });
  const DeviceRegistrationInfo = IDL.Record({
    'tentative_device' : IDL.Opt(DeviceData),
    'expiration' : Timestamp,
    'tentative_session' : IDL.Opt(IDL.Principal),
  });
  const IdentityAnchorInfo = IDL.Record({
    'name' : IDL.Opt(IDL.Text),
    'created_at' : IDL.Opt(Timestamp),
    'devices' : IDL.Vec(DeviceWithUsage),
    'openid_credentials' : IDL.Opt(IDL.Vec(OpenIdCredential)),
    'device_registration' : IDL.Opt(DeviceRegistrationInfo),
  });
  const GetAttributesRequest = IDL.Record({
    'origin' : FrontendHostname,
    'account_number' : IDL.Opt(AccountNumber),
    'attributes' : IDL.Vec(IDL.Tuple(IDL.Text, IDL.Vec(IDL.Nat8))),
    'issued_at_timestamp_ns' : Timestamp,
    'identity_number' : IdentityNumber,
  });
  const CertifiedAttribute = IDL.Record({
    'key' : IDL.Text,
    'signature' : IDL.Vec(IDL.Nat8),
    'value' : IDL.Vec(IDL.Nat8),
  });
  const CertifiedAttributes = IDL.Record({
    'expires_at_timestamp_ns' : Timestamp,
    'certified_attributes' : IDL.Vec(CertifiedAttribute),
  });
  const GetAccountError = IDL.Variant({
    'NoSuchOrigin' : IDL.Record({ 'anchor_number' : UserNumber }),
    'NoSuchAccount' : IDL.Record({
      'origin' : FrontendHostname,
      'anchor_number' : UserNumber,
    }),
  });
  const GetAttributesError = IDL.Variant({
    'AuthorizationError' : IDL.Principal,
    'ValidationError' : IDL.Record({ 'problems' : IDL.Vec(IDL.Text) }),
    'GetAccountError' : GetAccountError,
  });
  const GetDefaultAccountError = IDL.Variant({
    'NoSuchOrigin' : IDL.Record({ 'anchor_number' : UserNumber }),
    'NoSuchAnchor' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
  });
  const GetDelegationResponse = IDL.Variant({
    'no_such_delegation' : IDL.Null,
    'signed_delegation' : SignedDelegation,
  });
  const GetIcrc3AttributeRequest = IDL.Record({
    'origin' : FrontendHostname,
    'account_number' : IDL.Opt(AccountNumber),
    'message' : IDL.Vec(IDL.Nat8),
    'identity_number' : IdentityNumber,
  });
  const GetIcrc3AttributeResponse = IDL.Record({
    'signature' : IDL.Vec(IDL.Nat8),
  });
  const GetIcrc3AttributeError = IDL.Variant({
    'AuthorizationError' : IDL.Principal,
    'NoSuchSignature' : IDL.Null,
    'ValidationError' : IDL.Record({ 'problems' : IDL.Vec(IDL.Text) }),
    'GetAccountError' : GetAccountError,
  });
  const GetIdAliasRequest = IDL.Record({
    'rp_id_alias_jwt' : IDL.Text,
    'issuer' : FrontendHostname,
    'issuer_id_alias_jwt' : IDL.Text,
    'relying_party' : FrontendHostname,
    'identity_number' : IdentityNumber,
  });
  const SignedIdAlias = IDL.Record({
    'credential_jws' : IDL.Text,
    'id_alias' : IDL.Principal,
    'id_dapp' : IDL.Principal,
  });
  const IdAliasCredentials = IDL.Record({
    'rp_id_alias_credential' : SignedIdAlias,
    'issuer_id_alias_credential' : SignedIdAlias,
  });
  const GetIdAliasError = IDL.Variant({
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
    'NoSuchCredentials' : IDL.Text,
  });
  const SessionDelegationError = IDL.Variant({
    'NoSuchDelegation' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
  });
  const GetSsoDiscoveryStatusRequest = IDL.Record({
    'target_app_origin' : IDL.Opt(FrontendHostname),
    'org_domain' : IDL.Text,
  });
  const SsoDiscovery = IDL.Record({
    'scopes' : IDL.Vec(IDL.Text),
    'name' : IDL.Opt(IDL.Text),
    'authorization_endpoint' : IDL.Text,
    'issuer' : IDL.Text,
    'resolved_client_id' : IDL.Opt(IDL.Text),
    'discovery_domain' : IDL.Text,
    'client_id' : IDL.Text,
  });
  const SsoDiscoveryStatus = IDL.Variant({
    'Resolved' : SsoDiscovery,
    'Pending' : IDL.Null,
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
    'upgrade' : IDL.Opt(IDL.Bool),
    'status_code' : IDL.Nat16,
  });
  const IdentityAuthnInfo = IDL.Record({
    'authn_methods' : IDL.Vec(AuthnMethod),
    'recovery_authn_methods' : IDL.Vec(AuthnMethod),
  });
  const VerifiedEmail = IDL.Record({
    'address' : IDL.Text,
    'verified_at' : Timestamp,
  });
  const EmailRecoveryCredential = IDL.Record({
    'created_at' : Timestamp,
    'address' : IDL.Text,
    'last_used' : IDL.Opt(Timestamp),
  });
  const AuthnMethodRegistrationInfo = IDL.Record({
    'expiration' : Timestamp,
    'session' : IDL.Opt(IDL.Principal),
    'authn_method' : IDL.Opt(AuthnMethodData),
  });
  const IdentityInfo = IDL.Record({
    'authn_methods' : IDL.Vec(AuthnMethodData),
    'verified_emails' : IDL.Opt(IDL.Vec(VerifiedEmail)),
    'metadata' : MetadataMapV2,
    'name' : IDL.Opt(IDL.Text),
    'email_recovery' : IDL.Opt(IDL.Vec(EmailRecoveryCredential)),
    'created_at' : IDL.Opt(Timestamp),
    'authn_method_registration' : IDL.Opt(AuthnMethodRegistrationInfo),
    'openid_credentials' : IDL.Opt(IDL.Vec(OpenIdCredential)),
  });
  const IdentityInfoError = IDL.Variant({
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
  });
  const IdentityMetadataReplaceError = IDL.Variant({
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
    'StorageSpaceExceeded' : IDL.Record({
      'space_required' : IDL.Nat64,
      'space_available' : IDL.Nat64,
    }),
  });
  const IdentityPropertiesReplace = IDL.Record({ 'name' : IDL.Opt(IDL.Text) });
  const IdentityPropertiesReplaceError = IDL.Variant({
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
    'NameTooLong' : IDL.Record({ 'limit' : IDL.Nat64 }),
    'StorageSpaceExceeded' : IDL.Record({
      'space_required' : IDL.Nat64,
      'space_available' : IDL.Nat64,
    }),
  });
  const IdRegFinishArg = IDL.Record({
    'name' : IDL.Opt(IDL.Text),
    'authn_method' : AuthnMethodData,
  });
  const IdRegFinishResult = IDL.Record({ 'identity_number' : IDL.Nat64 });
  const IdRegFinishError = IDL.Variant({
    'SsoNormalLoginRequired' : IDL.Null,
    'NoRegistrationFlow' : IDL.Null,
    'UnexpectedCall' : IDL.Record({ 'next_step' : RegistrationFlowNextStep }),
    'InvalidAuthnMethod' : IDL.Text,
    'StorageError' : IDL.Text,
  });
  const IdRegStartError = IDL.Variant({
    'InvalidCaller' : IDL.Null,
    'AlreadyInProgress' : IDL.Null,
    'RateLimitExceeded' : IDL.Null,
  });
  const ListAvailableAttributesRequest = IDL.Record({
    'attributes' : IDL.Opt(IDL.Vec(IDL.Text)),
    'identity_number' : IdentityNumber,
  });
  const ListAvailableAttributesResponse = IDL.Vec(
    IDL.Tuple(IDL.Text, IDL.Vec(IDL.Nat8))
  );
  const ListAvailableAttributesError = IDL.Variant({
    'AuthorizationError' : IDL.Principal,
    'ValidationError' : IDL.Record({ 'problems' : IDL.Vec(IDL.Text) }),
  });
  const DeviceKeyWithAnchor = IDL.Record({
    'pubkey' : DeviceKey,
    'anchor_number' : UserNumber,
  });
  const McpConfig = IDL.Record({
    'url' : IDL.Opt(IDL.Text),
    'enabled' : IDL.Bool,
  });
  const McpPrepareDelegation = IDL.Record({
    'user_key' : UserKey,
    'account_number' : IDL.Opt(AccountNumber),
    'expiration' : Timestamp,
  });
  const McpRegistrationV2 = IDL.Record({
    'permissions' : Permissions,
    'expiration' : Timestamp,
  });
  const JWT = IDL.Text;
  const Salt = IDL.Vec(IDL.Nat8);
  const OpenIdCredentialAddError = IDL.Variant({
    'OpenIdCredentialAlreadyRegistered' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'JwtExpired' : IDL.Null,
    'Unauthorized' : IDL.Principal,
    'JwtVerificationFailed' : IDL.Null,
  });
  const OpenIdCredentialKey = IDL.Tuple(Iss, Sub, Aud);
  const OpenIdCredentialRemoveError = IDL.Variant({
    'InternalCanisterError' : IDL.Text,
    'OpenIdCredentialNotFound' : IDL.Null,
    'Unauthorized' : IDL.Principal,
  });
  const OpenIdDelegationError = IDL.Variant({
    'NoSuchDelegation' : IDL.Null,
    'NoSuchAnchor' : IDL.Null,
    'JwtExpired' : IDL.Null,
    'JwtVerificationFailed' : IDL.Null,
  });
  const OpenIDRegFinishArg = IDL.Record({
    'jwt' : JWT,
    'name' : IDL.Text,
    'origin' : IDL.Opt(IDL.Text),
    'salt' : Salt,
    'discovery_domain' : IDL.Opt(IDL.Text),
  });
  const OpenIdPrepareDelegationResponse = IDL.Record({
    'user_key' : UserKey,
    'expiration' : Timestamp,
    'anchor_number' : UserNumber,
  });
  const PrepareAccountDelegation = IDL.Record({
    'user_key' : UserKey,
    'expiration' : Timestamp,
  });
  const PrepareAttributeRequest = IDL.Record({
    'origin' : FrontendHostname,
    'attribute_keys' : IDL.Vec(IDL.Text),
    'account_number' : IDL.Opt(AccountNumber),
    'identity_number' : IdentityNumber,
  });
  const PrepareAttributeResponse = IDL.Record({
    'attributes' : IDL.Vec(IDL.Tuple(IDL.Text, IDL.Vec(IDL.Nat8))),
    'issued_at_timestamp_ns' : Timestamp,
  });
  const PrepareAttributeError = IDL.Variant({
    'AuthorizationError' : IDL.Principal,
    'ValidationError' : IDL.Record({ 'problems' : IDL.Vec(IDL.Text) }),
    'GetAccountError' : GetAccountError,
  });
  const AttributeSpec = IDL.Record({
    'key' : IDL.Text,
    'value' : IDL.Opt(IDL.Vec(IDL.Nat8)),
    'omit_scope' : IDL.Bool,
  });
  const PrepareIcrc3AttributeRequest = IDL.Record({
    'unmapped_origin' : IDL.Opt(FrontendHostname),
    'origin' : FrontendHostname,
    'account_number' : IDL.Opt(AccountNumber),
    'attributes' : IDL.Vec(AttributeSpec),
    'nonce' : IDL.Vec(IDL.Nat8),
    'identity_number' : IdentityNumber,
  });
  const PrepareIcrc3AttributeResponse = IDL.Record({
    'message' : IDL.Vec(IDL.Nat8),
  });
  const PrepareIcrc3AttributeError = IDL.Variant({
    'AuthorizationError' : IDL.Principal,
    'ValidationError' : IDL.Record({ 'problems' : IDL.Vec(IDL.Text) }),
    'GetAccountError' : GetAccountError,
    'AttributeMismatch' : IDL.Record({ 'problems' : IDL.Vec(IDL.Text) }),
  });
  const PrepareIdAliasRequest = IDL.Record({
    'issuer' : FrontendHostname,
    'relying_party' : FrontendHostname,
    'identity_number' : IdentityNumber,
  });
  const PreparedIdAlias = IDL.Record({
    'rp_id_alias_jwt' : IDL.Text,
    'issuer_id_alias_jwt' : IDL.Text,
    'canister_sig_pk_der' : PublicKey,
  });
  const PrepareIdAliasError = IDL.Variant({
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
  });
  const PrepareMcpRegistrationDelegation = IDL.Record({
    'user_key' : UserKey,
    'expiration' : Timestamp,
  });
  const PrepareSessionDelegation = IDL.Record({
    'user_key' : UserKey,
    'expiration' : Timestamp,
  });
  const ChallengeResult = IDL.Record({
    'key' : ChallengeKey,
    'chars' : IDL.Text,
  });
  const RegisterResponse = IDL.Variant({
    'bad_challenge' : IDL.Null,
    'canister_full' : IDL.Null,
    'registered' : IDL.Record({ 'user_number' : UserNumber }),
  });
  const SetDefaultAccountError = IDL.Variant({
    'NoSuchOrigin' : IDL.Record({ 'anchor_number' : UserNumber }),
    'NoSuchAnchor' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
    'NoSuchAccount' : IDL.Record({
      'origin' : FrontendHostname,
      'anchor_number' : UserNumber,
    }),
  });
  const SmtpAddress = IDL.Record({ 'domain' : IDL.Text, 'user' : IDL.Text });
  const SmtpEnvelope = IDL.Record({
    'to' : IDL.Vec(SmtpAddress),
    'from' : SmtpAddress,
  });
  const SmtpHeader = IDL.Record({ 'value' : IDL.Text, 'name' : IDL.Text });
  const SmtpMessage = IDL.Record({
    'body' : IDL.Vec(IDL.Nat8),
    'headers' : IDL.Vec(SmtpHeader),
  });
  const SmtpRequest = IDL.Record({
    'envelope' : IDL.Opt(SmtpEnvelope),
    'message' : IDL.Opt(SmtpMessage),
    'gateway_flags' : IDL.Opt(IDL.Vec(IDL.Text)),
    'message_id' : IDL.Opt(IDL.Text),
  });
  const SmtpRequestError = IDL.Record({
    'code' : IDL.Nat64,
    'message' : IDL.Text,
  });
  const SmtpResponse = IDL.Variant({
    'Ok' : IDL.Record({}),
    'Err' : SmtpRequestError,
  });
  const SsoGetDelegationRequest = IDL.Record({
    'jwt' : JWT,
    'session_key' : SessionKey,
    'salt' : Salt,
    'sso_attr_bundle' : IDL.Vec(IDL.Nat8),
    'target_app_origin' : FrontendHostname,
    'expiration' : Timestamp,
    'org_domain' : IDL.Text,
  });
  const SsoGetDelegationResponse = IDL.Record({
    'signed_delegation' : SignedDelegation,
    'sso_attr_bundle_signature' : IDL.Vec(IDL.Nat8),
  });
  const SsoPrepareDelegationRequest = IDL.Record({
    'jwt' : JWT,
    'session_key' : SessionKey,
    'salt' : Salt,
    'target_app_origin' : FrontendHostname,
    'org_domain' : IDL.Text,
  });
  const SsoPrepareDelegationResponse = IDL.Record({
    'user_key' : UserKey,
    'sso_attr_bundle' : IDL.Vec(IDL.Nat8),
    'expiration' : Timestamp,
    'anchor_number' : UserNumber,
  });
  const ArchiveInfo = IDL.Record({
    'archive_config' : IDL.Opt(ArchiveConfig),
    'archive_canister' : IDL.Opt(IDL.Principal),
  });
  const InternetIdentityStats = IDL.Record({
    'storage_layout_version' : IDL.Nat8,
    'users_registered' : IDL.Nat64,
    'assigned_user_number_range' : IDL.Tuple(IDL.Nat64, IDL.Nat64),
    'archive_info' : ArchiveInfo,
    'canister_creation_cycles_cost' : IDL.Nat64,
    'event_aggregations' : IDL.Vec(
      IDL.Tuple(IDL.Text, IDL.Vec(IDL.Tuple(IDL.Text, IDL.Nat64)))
    ),
  });
  const AccountUpdate = IDL.Record({ 'name' : IDL.Opt(IDL.Text) });
  const UpdateAccountError = IDL.Variant({
    'AccountLimitReached' : IDL.Null,
    'InternalCanisterError' : IDL.Text,
    'Unauthorized' : IDL.Principal,
    'NameTooLong' : IDL.Null,
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
    'authn_method_add' : IDL.Func(
        [IdentityNumber, AuthnMethodData],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : AuthnMethodAddError })],
        [],
      ),
    'authn_method_confirm' : IDL.Func(
        [IdentityNumber, IDL.Text],
        [
          IDL.Variant({
            'Ok' : IDL.Null,
            'Err' : AuthnMethodConfirmationError,
          }),
        ],
        [],
      ),
    'authn_method_metadata_replace' : IDL.Func(
        [IdentityNumber, PublicKey, MetadataMapV2],
        [
          IDL.Variant({
            'Ok' : IDL.Null,
            'Err' : AuthnMethodMetadataReplaceError,
          }),
        ],
        [],
      ),
    'authn_method_register' : IDL.Func(
        [IdentityNumber, AuthnMethodData],
        [
          IDL.Variant({
            'Ok' : AuthnMethodConfirmationCode,
            'Err' : AuthnMethodRegisterError,
          }),
        ],
        [],
      ),
    'authn_method_registration_mode_enter' : IDL.Func(
        [IdentityNumber, IDL.Opt(RegistrationId)],
        [
          IDL.Variant({
            'Ok' : IDL.Record({ 'expiration' : Timestamp }),
            'Err' : AuthnMethodRegistrationModeEnterError,
          }),
        ],
        [],
      ),
    'authn_method_registration_mode_exit' : IDL.Func(
        [IdentityNumber, IDL.Opt(AuthnMethodData)],
        [
          IDL.Variant({
            'Ok' : IDL.Null,
            'Err' : AuthnMethodRegistrationModeExitError,
          }),
        ],
        [],
      ),
    'authn_method_remove' : IDL.Func(
        [IdentityNumber, PublicKey],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : IDL.Null })],
        [],
      ),
    'authn_method_replace' : IDL.Func(
        [IdentityNumber, PublicKey, AuthnMethodData],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : AuthnMethodReplaceError })],
        [],
      ),
    'authn_method_security_settings_replace' : IDL.Func(
        [IdentityNumber, PublicKey, AuthnMethodSecuritySettings],
        [
          IDL.Variant({
            'Ok' : IDL.Null,
            'Err' : AuthnMethodSecuritySettingsReplaceError,
          }),
        ],
        [],
      ),
    'authn_method_session_info' : IDL.Func(
        [IdentityNumber],
        [IDL.Opt(AuthnMethodSessionInfo)],
        ['query'],
      ),
    'authn_method_session_register' : IDL.Func(
        [IdentityNumber],
        [
          IDL.Variant({
            'Ok' : AuthnMethodConfirmationCode,
            'Err' : AuthnMethodRegisterError,
          }),
        ],
        [],
      ),
    'check_captcha' : IDL.Func(
        [CheckCaptchaArg],
        [
          IDL.Variant({
            'Ok' : IdRegNextStepResult,
            'Err' : CheckCaptchaError,
          }),
        ],
        [],
      ),
    'config' : IDL.Func([], [InternetIdentityInit], ['query']),
    'create_account' : IDL.Func(
        [UserNumber, FrontendHostname, IDL.Text],
        [IDL.Variant({ 'Ok' : AccountInfo, 'Err' : CreateAccountError })],
        [],
      ),
    'create_challenge' : IDL.Func([], [Challenge], []),
    'deploy_archive' : IDL.Func([IDL.Vec(IDL.Nat8)], [DeployArchiveResult], []),
    'discover_sso' : IDL.Func([IDL.Text], [], []),
    'email_challenge_diagnostics' : IDL.Func(
        [IDL.Text],
        [IDL.Opt(EmailChallengeDiagnostics)],
        ['query'],
      ),
    'email_challenge_resolve_via_doh' : IDL.Func(
        [EmailChallengeResolveViaDohArg],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : EmailChallengeError })],
        [],
      ),
    'email_challenge_status' : IDL.Func(
        [IDL.Text],
        [EmailChallengeStatus],
        ['query'],
      ),
    'email_challenge_submit_dkim_leaf' : IDL.Func(
        [EmailChallengeSubmitDkimLeafArg],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : EmailChallengeError })],
        [],
      ),
    'email_recovery_credential_prepare_add' : IDL.Func(
        [IdentityNumber, EmailChallengeDnsInput],
        [IDL.Variant({ 'Ok' : EmailChallenge, 'Err' : EmailChallengeError })],
        [],
      ),
    'email_recovery_credential_remove' : IDL.Func(
        [IdentityNumber, IDL.Text],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : EmailChallengeError })],
        [],
      ),
    'email_recovery_diagnostics' : IDL.Func(
        [IDL.Text],
        [IDL.Opt(EmailChallengeDiagnostics)],
        ['query'],
      ),
    'email_recovery_get_delegation' : IDL.Func(
        [EmailRecoveryGetDelegationArgs],
        [IDL.Variant({ 'Ok' : SignedDelegation, 'Err' : EmailChallengeError })],
        ['query'],
      ),
    'email_recovery_prepare_delegation' : IDL.Func(
        [EmailChallengeDnsInput, SessionKey],
        [IDL.Variant({ 'Ok' : EmailChallenge, 'Err' : EmailChallengeError })],
        [],
      ),
    'email_recovery_resolve_via_doh' : IDL.Func(
        [EmailChallengeResolveViaDohArg],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : EmailChallengeError })],
        [],
      ),
    'email_recovery_status' : IDL.Func(
        [IDL.Text],
        [EmailChallengeStatus],
        ['query'],
      ),
    'email_recovery_submit_dkim_leaf' : IDL.Func(
        [EmailChallengeSubmitDkimLeafArg],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : EmailChallengeError })],
        [],
      ),
    'enter_device_registration_mode' : IDL.Func([UserNumber], [Timestamp], []),
    'exit_device_registration_mode' : IDL.Func([UserNumber], [], []),
    'fetch_entries' : IDL.Func([], [IDL.Vec(BufferedArchiveEntry)], []),
    'get_account_delegation' : IDL.Func(
        [
          UserNumber,
          FrontendHostname,
          IDL.Opt(AccountNumber),
          SessionKey,
          Timestamp,
          IDL.Opt(Permissions),
        ],
        [
          IDL.Variant({
            'Ok' : SignedDelegation,
            'Err' : AccountDelegationError,
          }),
        ],
        ['query'],
      ),
    'get_accounts' : IDL.Func(
        [UserNumber, FrontendHostname],
        [
          IDL.Variant({
            'Ok' : IDL.Vec(AccountInfo),
            'Err' : GetAccountsError,
          }),
        ],
        ['query'],
      ),
    'get_anchor_credentials' : IDL.Func(
        [UserNumber],
        [AnchorCredentials],
        ['query'],
      ),
    'get_anchor_info' : IDL.Func([UserNumber], [IdentityAnchorInfo], []),
    'get_attributes' : IDL.Func(
        [GetAttributesRequest],
        [
          IDL.Variant({
            'Ok' : CertifiedAttributes,
            'Err' : GetAttributesError,
          }),
        ],
        ['query'],
      ),
    'get_default_account' : IDL.Func(
        [UserNumber, FrontendHostname],
        [IDL.Variant({ 'Ok' : AccountInfo, 'Err' : GetDefaultAccountError })],
        ['query'],
      ),
    'get_delegation' : IDL.Func(
        [UserNumber, FrontendHostname, SessionKey, Timestamp],
        [GetDelegationResponse],
        ['query'],
      ),
    'get_icrc3_attributes' : IDL.Func(
        [GetIcrc3AttributeRequest],
        [
          IDL.Variant({
            'Ok' : GetIcrc3AttributeResponse,
            'Err' : GetIcrc3AttributeError,
          }),
        ],
        ['query'],
      ),
    'get_id_alias' : IDL.Func(
        [GetIdAliasRequest],
        [IDL.Variant({ 'Ok' : IdAliasCredentials, 'Err' : GetIdAliasError })],
        ['query'],
      ),
    'get_mcp_registration_delegation' : IDL.Func(
        [UserNumber, SessionKey, PublicKey, Timestamp],
        [IDL.Variant({ 'Ok' : SignedDelegation, 'Err' : IDL.Text })],
        ['query'],
      ),
    'get_principal' : IDL.Func(
        [UserNumber, FrontendHostname],
        [IDL.Principal],
        ['query'],
      ),
    'get_session_delegation' : IDL.Func(
        [UserNumber, SessionKey, Timestamp],
        [
          IDL.Variant({
            'Ok' : SignedDelegation,
            'Err' : SessionDelegationError,
          }),
        ],
        ['query'],
      ),
    'get_sso_discovery_status' : IDL.Func(
        [GetSsoDiscoveryStatusRequest],
        [SsoDiscoveryStatus],
        ['query'],
      ),
    'http_request' : IDL.Func([HttpRequest], [HttpResponse], ['query']),
    'identity_authn_info' : IDL.Func(
        [IdentityNumber],
        [IDL.Variant({ 'Ok' : IdentityAuthnInfo, 'Err' : IDL.Null })],
        ['query'],
      ),
    'identity_info' : IDL.Func(
        [IdentityNumber],
        [IDL.Variant({ 'Ok' : IdentityInfo, 'Err' : IdentityInfoError })],
        [],
      ),
    'identity_metadata_replace' : IDL.Func(
        [IdentityNumber, MetadataMapV2],
        [
          IDL.Variant({
            'Ok' : IDL.Null,
            'Err' : IdentityMetadataReplaceError,
          }),
        ],
        [],
      ),
    'identity_properties_replace' : IDL.Func(
        [IdentityNumber, IdentityPropertiesReplace],
        [
          IDL.Variant({
            'Ok' : IDL.Null,
            'Err' : IdentityPropertiesReplaceError,
          }),
        ],
        [],
      ),
    'identity_registration_finish' : IDL.Func(
        [IdRegFinishArg],
        [IDL.Variant({ 'Ok' : IdRegFinishResult, 'Err' : IdRegFinishError })],
        [],
      ),
    'identity_registration_start' : IDL.Func(
        [],
        [IDL.Variant({ 'Ok' : IdRegNextStepResult, 'Err' : IdRegStartError })],
        [],
      ),
    'init_salt' : IDL.Func([], [], []),
    'list_available_attributes' : IDL.Func(
        [ListAvailableAttributesRequest],
        [
          IDL.Variant({
            'Ok' : ListAvailableAttributesResponse,
            'Err' : ListAvailableAttributesError,
          }),
        ],
        ['query'],
      ),
    'lookup' : IDL.Func([UserNumber], [IDL.Vec(DeviceData)], ['query']),
    'lookup_by_registration_mode_id' : IDL.Func(
        [RegistrationId],
        [IDL.Opt(IdentityNumber)],
        ['query'],
      ),
    'lookup_caller_identity_by_recovery_phrase' : IDL.Func(
        [],
        [IDL.Opt(IdentityNumber)],
        [],
      ),
    'lookup_device_key' : IDL.Func(
        [IDL.Vec(IDL.Nat8)],
        [IDL.Opt(DeviceKeyWithAnchor)],
        ['query'],
      ),
    'mcp_get_accounts' : IDL.Func(
        [FrontendHostname],
        [
          IDL.Variant({
            'Ok' : IDL.Vec(AccountInfo),
            'Err' : AccountDelegationError,
          }),
        ],
        ['query'],
      ),
    'mcp_get_config' : IDL.Func([UserNumber], [McpConfig], ['query']),
    'mcp_get_delegation' : IDL.Func(
        [FrontendHostname, IDL.Opt(AccountNumber), SessionKey, Timestamp],
        [
          IDL.Variant({
            'Ok' : SignedDelegation,
            'Err' : AccountDelegationError,
          }),
        ],
        ['query'],
      ),
    'mcp_prepare_delegation' : IDL.Func(
        [
          FrontendHostname,
          IDL.Opt(AccountNumber),
          SessionKey,
          IDL.Opt(IDL.Nat64),
        ],
        [
          IDL.Variant({
            'Ok' : McpPrepareDelegation,
            'Err' : AccountDelegationError,
          }),
        ],
        [],
      ),
    'mcp_register_v2' : IDL.Func(
        [SessionKey],
        [IDL.Variant({ 'Ok' : McpRegistrationV2, 'Err' : IDL.Text })],
        [],
      ),
    'mcp_set_config' : IDL.Func(
        [UserNumber, McpConfig],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : IDL.Text })],
        [],
      ),
    'openid_credential_add' : IDL.Func(
        [IdentityNumber, JWT, Salt, IDL.Opt(IDL.Text)],
        [
          IDL.Variant({
            'Ok' : IDL.Null,
            'Err' : OpenIdCredentialAddError,
            'Pending' : IDL.Null,
          }),
        ],
        [],
      ),
    'openid_credential_remove' : IDL.Func(
        [IdentityNumber, OpenIdCredentialKey],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : OpenIdCredentialRemoveError })],
        [],
      ),
    'openid_get_delegation' : IDL.Func(
        [JWT, Salt, SessionKey, Timestamp, IDL.Opt(IDL.Text)],
        [
          IDL.Variant({
            'Ok' : SignedDelegation,
            'Err' : OpenIdDelegationError,
            'Pending' : IDL.Null,
          }),
        ],
        ['query'],
      ),
    'openid_identity_registration_finish' : IDL.Func(
        [OpenIDRegFinishArg],
        [
          IDL.Variant({
            'Ok' : IdRegFinishResult,
            'Err' : IdRegFinishError,
            'Pending' : IDL.Null,
          }),
        ],
        [],
      ),
    'openid_prepare_delegation' : IDL.Func(
        [JWT, Salt, SessionKey, IDL.Opt(IDL.Text)],
        [
          IDL.Variant({
            'Ok' : OpenIdPrepareDelegationResponse,
            'Err' : OpenIdDelegationError,
            'Pending' : IDL.Null,
          }),
        ],
        [],
      ),
    'prepare_account_delegation' : IDL.Func(
        [
          UserNumber,
          FrontendHostname,
          IDL.Opt(AccountNumber),
          SessionKey,
          IDL.Opt(IDL.Nat64),
          IDL.Opt(Permissions),
        ],
        [
          IDL.Variant({
            'Ok' : PrepareAccountDelegation,
            'Err' : AccountDelegationError,
          }),
        ],
        [],
      ),
    'prepare_attributes' : IDL.Func(
        [PrepareAttributeRequest],
        [
          IDL.Variant({
            'Ok' : PrepareAttributeResponse,
            'Err' : PrepareAttributeError,
          }),
        ],
        [],
      ),
    'prepare_delegation' : IDL.Func(
        [UserNumber, FrontendHostname, SessionKey, IDL.Opt(IDL.Nat64)],
        [UserKey, Timestamp],
        [],
      ),
    'prepare_icrc3_attributes' : IDL.Func(
        [PrepareIcrc3AttributeRequest],
        [
          IDL.Variant({
            'Ok' : PrepareIcrc3AttributeResponse,
            'Err' : PrepareIcrc3AttributeError,
          }),
        ],
        [],
      ),
    'prepare_id_alias' : IDL.Func(
        [PrepareIdAliasRequest],
        [IDL.Variant({ 'Ok' : PreparedIdAlias, 'Err' : PrepareIdAliasError })],
        [],
      ),
    'prepare_mcp_registration_delegation' : IDL.Func(
        [UserNumber, SessionKey, IDL.Opt(Permissions), IDL.Opt(IDL.Nat64)],
        [
          IDL.Variant({
            'Ok' : PrepareMcpRegistrationDelegation,
            'Err' : IDL.Text,
          }),
        ],
        [],
      ),
    'prepare_session_delegation' : IDL.Func(
        [UserNumber, SessionKey, IDL.Opt(IDL.Nat64)],
        [
          IDL.Variant({
            'Ok' : PrepareSessionDelegation,
            'Err' : SessionDelegationError,
          }),
        ],
        [],
      ),
    'register' : IDL.Func(
        [DeviceData, ChallengeResult, IDL.Opt(IDL.Principal)],
        [RegisterResponse],
        [],
      ),
    'remove' : IDL.Func([UserNumber, DeviceKey], [], []),
    'replace' : IDL.Func([UserNumber, DeviceKey, DeviceData], [], []),
    'set_default_account' : IDL.Func(
        [UserNumber, FrontendHostname, IDL.Opt(AccountNumber)],
        [IDL.Variant({ 'Ok' : AccountInfo, 'Err' : SetDefaultAccountError })],
        [],
      ),
    'smtp_request' : IDL.Func([SmtpRequest], [SmtpResponse], []),
    'smtp_request_validate' : IDL.Func(
        [SmtpRequest],
        [SmtpResponse],
        ['query'],
      ),
    'sso_get_delegation' : IDL.Func(
        [SsoGetDelegationRequest],
        [
          IDL.Variant({
            'Ok' : SsoGetDelegationResponse,
            'Err' : OpenIdDelegationError,
            'Pending' : IDL.Null,
          }),
        ],
        ['query'],
      ),
    'sso_prepare_delegation' : IDL.Func(
        [SsoPrepareDelegationRequest],
        [
          IDL.Variant({
            'Ok' : SsoPrepareDelegationResponse,
            'Err' : OpenIdDelegationError,
            'Pending' : IDL.Null,
          }),
        ],
        [],
      ),
    'stats' : IDL.Func([], [InternetIdentityStats], ['query']),
    'update' : IDL.Func([UserNumber, DeviceKey, DeviceData], [], []),
    'update_account' : IDL.Func(
        [UserNumber, FrontendHostname, IDL.Opt(AccountNumber), AccountUpdate],
        [IDL.Variant({ 'Ok' : AccountInfo, 'Err' : UpdateAccountError })],
        [],
      ),
    'verified_email_prepare_add' : IDL.Func(
        [IdentityNumber, EmailChallengeDnsInput],
        [IDL.Variant({ 'Ok' : EmailChallenge, 'Err' : EmailChallengeError })],
        [],
      ),
    'verified_email_remove' : IDL.Func(
        [IdentityNumber, IDL.Text],
        [IDL.Variant({ 'Ok' : IDL.Null, 'Err' : EmailChallengeError })],
        [],
      ),
    'verify_tentative_device' : IDL.Func(
        [UserNumber, IDL.Text],
        [VerifyTentativeDeviceResponse],
        [],
      ),
    'whoami' : IDL.Func([], [IDL.Principal], ['query']),
  });
};
export const init = ({ IDL }) => {
  const DohConfig = IDL.Record({
    'max_cache_age_secs' : IDL.Opt(IDL.Nat64),
    'allowed_domains' : IDL.Vec(IDL.Text),
  });
  const SsoCredentialMigrationEntry = IDL.Record({
    'name' : IDL.Opt(IDL.Text),
    'issuer' : IDL.Text,
    'discovery_domain' : IDL.Text,
    'client_id' : IDL.Text,
  });
  const DnssecRootAnchor = IDL.Record({
    'algorithm' : IDL.Nat8,
    'key_tag' : IDL.Nat16,
    'digest_type' : IDL.Nat8,
    'digest' : IDL.Vec(IDL.Nat8),
  });
  const DnssecConfig = IDL.Record({
    'root_anchors' : IDL.Vec(DnssecRootAnchor),
  });
  const ArchiveConfig = IDL.Record({
    'polling_interval_ns' : IDL.Nat64,
    'entries_buffer_limit' : IDL.Nat64,
    'module_hash' : IDL.Vec(IDL.Nat8),
    'entries_fetch_limit' : IDL.Nat16,
  });
  const AnalyticsConfig = IDL.Variant({
    'Plausible' : IDL.Record({
      'domain' : IDL.Opt(IDL.Text),
      'track_localhost' : IDL.Opt(IDL.Bool),
      'hash_mode' : IDL.Opt(IDL.Bool),
      'api_host' : IDL.Opt(IDL.Text),
    }),
  });
  const OpenIdEmailVerification = IDL.Variant({
    'Google' : IDL.Null,
    'Unknown' : IDL.Null,
    'Microsoft' : IDL.Null,
  });
  const OpenIdConfig = IDL.Record({
    'auth_uri' : IDL.Text,
    'jwks_uri' : IDL.Text,
    'logo' : IDL.Text,
    'name' : IDL.Text,
    'fedcm_uri' : IDL.Opt(IDL.Text),
    'email_verification' : IDL.Opt(OpenIdEmailVerification),
    'issuer' : IDL.Text,
    'auth_scope' : IDL.Vec(IDL.Text),
    'seed_jwks' : IDL.Opt(IDL.Vec(IDL.Vec(IDL.Tuple(IDL.Text, IDL.Text)))),
    'client_id' : IDL.Text,
  });
  const CaptchaConfig = IDL.Record({
    'max_unsolved_captchas' : IDL.Nat64,
    'captcha_trigger' : IDL.Variant({
      'Dynamic' : IDL.Record({
        'reference_rate_sampling_interval_s' : IDL.Nat64,
        'threshold_pct' : IDL.Nat16,
        'current_rate_sampling_interval_s' : IDL.Nat64,
      }),
      'Static' : IDL.Variant({
        'CaptchaDisabled' : IDL.Null,
        'CaptchaEnabled' : IDL.Null,
      }),
    }),
  });
  const DummyAuthConfig = IDL.Record({ 'prompt_for_index' : IDL.Bool });
  const RateLimitConfig = IDL.Record({
    'max_tokens' : IDL.Nat64,
    'time_per_token_ns' : IDL.Nat64,
  });
  const InternetIdentityInit = IDL.Record({
    'doh_config' : IDL.Opt(IDL.Opt(DohConfig)),
    'sso_credential_migration' : IDL.Opt(IDL.Vec(SsoCredentialMigrationEntry)),
    'is_production' : IDL.Opt(IDL.Bool),
    'backend_canister_id' : IDL.Opt(IDL.Principal),
    'enable_dapps_explorer' : IDL.Opt(IDL.Bool),
    'assigned_user_number_range' : IDL.Opt(IDL.Tuple(IDL.Nat64, IDL.Nat64)),
    'new_flow_origins' : IDL.Opt(IDL.Vec(IDL.Text)),
    'dnssec_config' : IDL.Opt(IDL.Opt(DnssecConfig)),
    'archive_config' : IDL.Opt(ArchiveConfig),
    'canister_creation_cycles_cost' : IDL.Opt(IDL.Nat64),
    'analytics_config' : IDL.Opt(IDL.Opt(AnalyticsConfig)),
    'enable_dnssec_email_recovery' : IDL.Opt(IDL.Bool),
    'related_origins' : IDL.Opt(IDL.Vec(IDL.Text)),
    'openid_configs' : IDL.Opt(IDL.Vec(OpenIdConfig)),
    'backend_origin' : IDL.Opt(IDL.Text),
    'captcha_config' : IDL.Opt(CaptchaConfig),
    'dummy_auth' : IDL.Opt(IDL.Opt(DummyAuthConfig)),
    'sso_allow_insecure_discovery' : IDL.Opt(IDL.Bool),
    'register_rate_limit' : IDL.Opt(RateLimitConfig),
  });
  return [IDL.Opt(InternetIdentityInit)];
};
