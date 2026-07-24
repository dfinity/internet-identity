import IdentityAttributes "mo:identity-attributes";
import Map "mo:core/Map";
import Principal "mo:core/Principal";
import Time "mo:core/Time";
import Int "mo:core/Int";
import Result "mo:core/Result";

/// A relying-party test app that **gates a protected resource on a
/// certified Internet Identity SSO attribute**.
///
/// It exists to exercise the II role-based access-enforcement feature
/// end to end: II only certifies the `sso:<domain>` org-membership
/// attribute for a caller who completed a fresh SSO ceremony that
/// granted them a role for this app. This canister then only hands out
/// the protected resource to callers who present such a certified
/// bundle. A caller with no SSO attribute — a passkey/CLI/MCP session,
/// or a user the org's IdP refused (no role) — is turned away.
///
/// ## How enforcement works here
///
/// The `mo:identity-attributes` mixin injects the two sign-in methods
/// the frontend drives (`_internet_identity_sign_in_start` /
/// `_internet_identity_sign_in_finish`) and, for every bundle that
/// passes *its* checks (trusted signer — verified by the IC —, allowed
/// `frontend_origins`, single-use nonce, freshness, uniform attribute
/// sourcing), runs our `onVerified` callback with `{ name; email; sso }`.
///
/// `sso` is the matched trusted SSO domain when name/email came from
/// `sso:<domain>:*` keys, and `null` otherwise. The mixin itself
/// *accepts* a non-SSO bundle (it just reports `sso = null`), so the
/// SSO requirement is ours to impose: `getProtectedResource` grants
/// access only when the caller's verified session carries a non-null
/// `sso` domain.
///
/// ## Configuration (see `icp.yaml`)
///
/// - `trusted_attribute_signers` — the II canister principal whose
///   signature the IC must have verified on the bundle.
/// - `frontend_origins` — the exact origin(s) allowed in
///   `implicit:origin`; this is the "trusted domains" allowlist and
///   includes the app's Cloudflare-tunnel origin.
/// - `trusted_sso_domains` — the org SSO domain(s) accepted for
///   `sso:<domain>:*` keys. **Must be set**, or the library rejects
///   every SSO bundle as `#UntrustedSsoSource`.
persistent actor SsoGatedApp {

  /// What we remember about a caller once they've completed a verified
  /// sign-in. `sso` is the matched trusted domain, or `null` for a
  /// non-SSO (unscoped/openid) bundle.
  public type Session = {
    name : ?Text;
    email : ?Text;
    sso : ?Text;
    verifiedAtNs : Nat;
  };

  /// Why a protected call was refused.
  public type AccessError = {
    /// The caller has no verified session on this canister — they
    /// never completed `_internet_identity_sign_in_finish`, or did so
    /// under a different principal.
    #NotAuthenticated;
    /// The caller has a verified session, but it carries no SSO
    /// attribute (the bundle was non-SSO). This is the "org member
    /// without a granted role for this app" / "signed in without SSO"
    /// case that enforcement is designed to turn away.
    #SsoRequired;
  };

  /// Verified sessions, keyed by the principal that called
  /// `_internet_identity_sign_in_finish`. `transient` state in the
  /// mixin (the nonce store) is fine to lose on upgrade, but sessions
  /// are the app's own data, so we keep them stable.
  let sessions = Map.empty<Principal, Session>();

  // Injects `_internet_identity_sign_in_start` / `_finish`. Our
  // callback records the verified attributes so the gated endpoints
  // below can consult them.
  include IdentityAttributes({
    onVerified = func(caller : Principal, attrs : { name : ?Text; email : ?Text; sso : ?Text }) {
      let session : Session = {
        name = attrs.name;
        email = attrs.email;
        sso = attrs.sso;
        verifiedAtNs = Int.abs(Time.now());
      };
      Map.add(sessions, Principal.compare, caller, session);
    };
  });

  /// The gated resource. Returns the secret only to a caller whose
  /// verified session carries a trusted SSO domain; everyone else is
  /// refused with a reason.
  public shared query ({ caller }) func getProtectedResource() : async Result.Result<Text, AccessError> {
    switch (Map.get(sessions, Principal.compare, caller)) {
      case null #err(#NotAuthenticated);
      case (?session) {
        switch (session.sso) {
          case null #err(#SsoRequired);
          case (?domain) #ok("Access granted — verified member of " # domain # ".");
        };
      };
    };
  };

  /// Debug helper: what this canister recorded for the caller (if
  /// anything). Handy for seeing why `getProtectedResource` allowed or
  /// refused a given identity.
  public shared query ({ caller }) func sessionInfo() : async ?Session {
    Map.get(sessions, Principal.compare, caller)
  };

  /// Debug helper: the caller's principal as seen by this canister.
  public shared query ({ caller }) func whoami() : async Principal {
    caller
  };
};
