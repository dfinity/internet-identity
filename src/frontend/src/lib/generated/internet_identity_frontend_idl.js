export const idlFactory = ({ IDL }) => {
  const AnalyticsConfig = IDL.Variant({
    'Plausible' : IDL.Record({
      'domain' : IDL.Opt(IDL.Text),
      'track_localhost' : IDL.Opt(IDL.Bool),
      'hash_mode' : IDL.Opt(IDL.Bool),
      'api_host' : IDL.Opt(IDL.Text),
    }),
  });
  const DummyAuthConfig = IDL.Record({ 'prompt_for_index' : IDL.Bool });
  const InternetIdentityFrontendInit = IDL.Record({
    'fetch_root_key' : IDL.Opt(IDL.Bool),
    'backend_canister_id' : IDL.Principal,
    'analytics_config' : IDL.Opt(IDL.Opt(AnalyticsConfig)),
    'related_origins' : IDL.Opt(IDL.Vec(IDL.Text)),
    'backend_origin' : IDL.Text,
    'dev_csp' : IDL.Opt(IDL.Bool),
    'dummy_auth' : IDL.Opt(IDL.Opt(DummyAuthConfig)),
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
  return IDL.Service({
    'http_request' : IDL.Func([HttpRequest], [HttpResponse], ['query']),
  });
};
export const init = ({ IDL }) => {
  const AnalyticsConfig = IDL.Variant({
    'Plausible' : IDL.Record({
      'domain' : IDL.Opt(IDL.Text),
      'track_localhost' : IDL.Opt(IDL.Bool),
      'hash_mode' : IDL.Opt(IDL.Bool),
      'api_host' : IDL.Opt(IDL.Text),
    }),
  });
  const DummyAuthConfig = IDL.Record({ 'prompt_for_index' : IDL.Bool });
  const InternetIdentityFrontendInit = IDL.Record({
    'fetch_root_key' : IDL.Opt(IDL.Bool),
    'backend_canister_id' : IDL.Principal,
    'analytics_config' : IDL.Opt(IDL.Opt(AnalyticsConfig)),
    'related_origins' : IDL.Opt(IDL.Vec(IDL.Text)),
    'backend_origin' : IDL.Text,
    'dev_csp' : IDL.Opt(IDL.Bool),
    'dummy_auth' : IDL.Opt(IDL.Opt(DummyAuthConfig)),
  });
  return [InternetIdentityFrontendInit];
};
