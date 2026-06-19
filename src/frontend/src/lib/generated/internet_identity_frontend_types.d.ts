import type { Principal } from '@icp-sdk/core/principal';
import type { ActorMethod } from '@icp-sdk/core/agent';
import type { IDL } from '@icp-sdk/core/candid';

export type AnalyticsConfig = {
    'Plausible' : {
      'domain' : [] | [string],
      'track_localhost' : [] | [boolean],
      'hash_mode' : [] | [boolean],
      'api_host' : [] | [string],
    }
  };
export interface DummyAuthConfig {
  /**
   * Prompts user for a index value (0 - 255) when set to true,
   * this is used in e2e to have multiple dummy auth identities.
   */
  'prompt_for_index' : boolean,
}
export type HeaderField = [string, string];
export interface HttpRequest {
  'url' : string,
  'method' : string,
  'body' : Uint8Array | number[],
  'headers' : Array<HeaderField>,
  'certificate_version' : [] | [number],
}
export interface HttpResponse {
  'body' : Uint8Array | number[],
  'headers' : Array<HeaderField>,
  'upgrade' : [] | [boolean],
  'status_code' : number,
}
export interface InternetIdentityFrontendInit {
  'fetch_root_key' : [] | [boolean],
  /**
   * Origins of apps to feature on the dashboard home. Each origin is resolved
   * against the bundled dapps catalogue to render name, description and logo.
   */
  'featured_dashboard_apps' : [] | [Array<string>],
  'backend_canister_id' : Principal,
  'analytics_config' : [] | [[] | [AnalyticsConfig]],
  'related_origins' : [] | [Array<string>],
  'backend_origin' : string,
  'dev_csp' : [] | [boolean],
  'dummy_auth' : [] | [[] | [DummyAuthConfig]],
  /**
   * Origin of the trusted MCP server, e.g. "https://mcp.id.ai" (no trailing
   * slash). The /mcp delegation flow delivers the delegation to this origin
   * only (it's added to the form-action CSP and /mcp rejects callbacks on any
   * other origin). When unset, the /mcp flow is disabled.
   */
  'mcp_server_origin' : [] | [string],
  /**
   * Frontend feature flag overrides keyed by flag name, e.g.
   * record { "EMAIL_RECOVERY"; true }. Sets the deployment-level baseline for
   * each flag; localStorage, the flag's init callback and ?feature_flag_* URL
   * params still take precedence. Unknown flag names are ignored.
   */
  'feature_flags' : [] | [Array<[string, boolean]>],
}
export interface StreamingCallbackHttpResponse {
  'token' : [] | [Token],
  'body' : Uint8Array | number[],
}
export type StreamingStrategy = {
    'Callback' : { 'token' : Token, 'callback' : [Principal, string] }
  };
export type Token = {};
export interface _SERVICE {
  'http_request' : ActorMethod<[HttpRequest], HttpResponse>,
}
export declare const idlFactory: IDL.InterfaceFactory;
export declare const init: (args: { IDL: typeof IDL }) => IDL.Type[];
