import II         "mo:identity-attributes";
import Challenges "mo:identity-attributes/Challenges";

import Map        "mo:core/Map";
import Principal  "mo:core/Principal";
import Text       "mo:core/Text";
import Time       "mo:core/Time";
import Int        "mo:core/Int";
import Nat        "mo:core/Nat";
import Nat8       "mo:core/Nat8";
import Array      "mo:core/Array";
import Iter       "mo:core/Iter";
import Order      "mo:core/Order";
import Result     "mo:core/Result";
import Random     "mo:core/Random";
import Blob       "mo:core/Blob";

/// Dfinsight — a "common matters of interest" board.
///
/// Users sign in 1-click via `https://id.ai/authorize?sso=dfinity.org`,
/// post one Issue per 24h, and upvote others'. They never see scores.
/// Admins (DFINITY members on a fixed allowlist) sign in with the same
/// SSO flow plus the `sso:dfinity.org:name` attribute, which the
/// canister verifies via `mo:identity-attributes` (which itself reads
/// the IC `sender_info` from `mo:core/CallerAttributes`).
persistent actor Dfinsight {

  // --------------------------------------------------------------- config --

  // Origin of *this* dapp (the relying party) — must match the
  // `implicit:origin` field that id.ai bakes into attribute bundles.
  //
  // Defaults to the local Vite dev server. Before deploying to
  // mainnet, edit this string to your frontend canister URL, e.g.
  // `"https://<frontend-id>.icp0.io"`. Mismatch produces
  // `#OriginMismatch` from `mo:identity-attributes` on every admin
  // verify, which is loud and easy to diagnose.
  let rpOrigin : Text = "http://localhost:5173";

  // 5-minute windows are the same defaults the bagel demo uses.
  let nonceTtlNs   : Nat = 5 * 60 * 1_000_000_000;
  let maxAttrAgeNs : Nat = 5 * 60 * 1_000_000_000;

  // After a successful Authorization-tier verify we keep the principal
  // marked as "trusted admin" for this long — so a single sign-in
  // covers a normal admin session (list, delete, respond, refresh)
  // without forcing a new II popup per click.
  let adminSessionNs : Nat = 30 * 60 * 1_000_000_000;

  // One issue per user per rolling 24h.
  let dayNs : Nat = 24 * 60 * 60 * 1_000_000_000;

  // Issue body length — keep it tweet-sized.
  let maxBodyChars : Nat = 280;

  // -------------------------------------------------------------- state --

  let nonces = Challenges.empty();

  var nextIssueId : Nat = 0;

  public type Issue = {
    id        : Nat;
    body      : Text;
    author    : Principal;
    createdAt : Int;
    upvotes   : Nat;
    response  : ?Text;
    deleted   : Bool;
  };

  let issues   = Map.empty<Nat, Issue>();
  // Per-issue voter sets — separate from `Issue` so an upvote toggle is
  // O(log n) instead of rebuilding the whole record.
  let upvoters = Map.empty<Nat, Map.Map<Principal, ()>>();
  // For the rolling-24h limit.
  let lastPostAt = Map.empty<Principal, Int>();

  // The admin list is public — anyone (signed in or not) can read it
  // via `listAdmins()`, so non-admin DFINITY members who hit the admin
  // page see who *is* allowed. Names must match the verified
  // `sso:dfinity.org:name` value exactly.
  var admins : [Text] = ["Arshavir Ter-Gabrielyan"];

  // Cache of `principal -> (verifiedName, expiresAt)` populated by
  // `establishAdminSession`. Subsequent admin actions just look up this
  // map rather than redoing the full attribute verify (which would
  // require a fresh nonce and a new SSO popup per click).
  let adminSessions = Map.empty<Principal, (Text, Int)>();

  // ------------------------------------------------------------ types --

  public type IssueForUser = {
    id        : Nat;
    body      : Text;
    upvoted   : Bool;
    response  : ?Text;
    // Surfaced only after an admin response — once the matter is
    // resolved the bias-protection no longer applies.
    upvotes   : ?Nat;
    votesLocked : Bool;
  };

  public type IssueForAdmin = {
    id       : Nat;
    body     : Text;
    upvotes  : Nat;
    response : ?Text;
    createdAt : Int;
  };

  public type PostError = {
    #NotSignedIn;
    #Empty;
    #TooLong : { maxChars : Nat };
    #DailyLimit : { nextAllowedNs : Int };
  };

  public type VoteError = {
    #NotSignedIn;
    #NotFound;
    #Deleted;
    #VotesLocked;
  };

  public type AdminError = {
    #Verify     : II.Error;
    #NoName;
    #NotAdmin   : { name : Text; admins : [Text] };
    #NotFound;
    #Empty;
    // Returned when an admin method is called without a live admin
    // session — the frontend should call `establishAdminSession` (which
    // does the full SSO + attribute verify) and then retry.
    #SessionExpired;
  };

  // -------------------------------------------------------- user API --

  /// 32-byte canister-issued nonce, scoped to `caller`. Required for the
  /// admin flow's Authorization-tier verify, but harmless (and free) for
  /// the user flow — we expose the same entry point so the frontend
  /// doesn't have to branch on "am I about to ask for attributes?".
  public shared ({ caller }) func generate_nonce() : async Blob {
    await Challenges.issue<system>(nonces, caller, nonceTtlNs);
  };

  /// Anyone signed in (i.e. non-anonymous principal — either an SSO
  /// delegation from id.ai, or any other authenticated identity) can
  /// post one Issue per 24h.
  public shared ({ caller }) func createIssue(body : Text) : async Result.Result<Nat, PostError> {
    if (Principal.isAnonymous(caller)) return #err(#NotSignedIn);

    let trimmed = Text.trim(body, #char ' ');
    if (Text.size(trimmed) == 0) return #err(#Empty);
    if (Text.size(trimmed) > maxBodyChars) return #err(#TooLong { maxChars = maxBodyChars });

    let now = Time.now();
    switch (Map.get(lastPostAt, Principal.compare, caller)) {
      case (?last) {
        let elapsed = Int.abs(now - last);
        if (elapsed < dayNs) {
          return #err(#DailyLimit { nextAllowedNs = last + dayNs });
        };
      };
      case null {};
    };

    let id = nextIssueId;
    nextIssueId += 1;
    Map.add(issues, Nat.compare, id, {
      id;
      body      = trimmed;
      author    = caller;
      createdAt = now;
      upvotes   = 0;
      response  = null;
      deleted   = false;
    });
    Map.add(upvoters, Nat.compare, id, Map.empty<Principal, ()>());
    Map.add(lastPostAt, Principal.compare, caller, now);
    #ok id
  };

  /// Toggle an upvote. The frontend doesn't see the absolute count
  /// (until the admin responds), only `upvoted : Bool`.
  public shared ({ caller }) func toggleUpvote(id : Nat) : async Result.Result<{ upvoted : Bool }, VoteError> {
    if (Principal.isAnonymous(caller)) return #err(#NotSignedIn);

    let ?issue = Map.get(issues, Nat.compare, id) else return #err(#NotFound);
    if (issue.deleted) return #err(#Deleted);
    switch (issue.response) {
      case (?_) return #err(#VotesLocked);
      case null {};
    };

    let voters = switch (Map.get(upvoters, Nat.compare, id)) {
      case (?v) v;
      case null {
        let fresh = Map.empty<Principal, ()>();
        Map.add(upvoters, Nat.compare, id, fresh);
        fresh;
      };
    };

    let alreadyVoted = Map.contains(voters, Principal.compare, caller);
    if (alreadyVoted) {
      Map.remove(voters, Principal.compare, caller);
      Map.add(issues, Nat.compare, id, { issue with upvotes = issue.upvotes - 1 });
      #ok({ upvoted = false })
    } else {
      Map.add(voters, Principal.compare, caller, ());
      Map.add(issues, Nat.compare, id, { issue with upvotes = issue.upvotes + 1 });
      #ok({ upvoted = true })
    }
  };

  /// User-facing list. Shuffled with on-chain entropy so position can't
  /// be used to infer ranking. Scores hidden unless the issue has been
  /// responded to by an admin.
  public shared ({ caller }) func listIssuesForUser() : async [IssueForUser] {
    let active = Iter.toArray(
      Iter.filter<Issue>(Map.values(issues), func(i) = not i.deleted)
    );

    let entropy = await Random.blob();
    let shuffled = shuffle(active, entropy);

    Array.map<Issue, IssueForUser>(shuffled, func(i) {
      let voted = switch (Map.get(upvoters, Nat.compare, i.id)) {
        case (?v) Map.contains(v, Principal.compare, caller);
        case null false;
      };
      let responded = switch (i.response) { case (?_) true; case null false };
      {
        id          = i.id;
        body        = i.body;
        upvoted     = voted;
        response    = i.response;
        upvotes     = if (responded) ?i.upvotes else null;
        votesLocked = responded;
      }
    })
  };

  /// Tells the frontend whether the user can post right now, and if not
  /// when they'll be allowed. Saves a round-trip on the post path.
  public shared query ({ caller }) func myPostStatus() : async {
    allowed       : Bool;
    nextAllowedNs : ?Int;
  } {
    if (Principal.isAnonymous(caller)) {
      return { allowed = false; nextAllowedNs = null };
    };
    switch (Map.get(lastPostAt, Principal.compare, caller)) {
      case null { { allowed = true; nextAllowedNs = null } };
      case (?last) {
        let elapsed = Int.abs(Time.now() - last);
        if (elapsed >= dayNs) { { allowed = true; nextAllowedNs = null } }
        else { { allowed = false; nextAllowedNs = ?(last + dayNs) } }
      };
    }
  };

  /// Public — anyone, signed in or not, can see who's allowed to admin.
  public query func listAdmins() : async [Text] { admins };

  // ------------------------------------------------------- admin API --

  // Full Authorization-tier verify against `mo:identity-attributes`.
  // Called only by `establishAdminSession`. Returns the verified name
  // on success.
  func verifyAdminAttributes(caller : Principal) : Result.Result<Text, AdminError> {
    let attrs = switch (II.verify<system>({
      policy = #Authorization {
        expectedOrigin = rpOrigin;
        maxAgeNs       = maxAttrAgeNs;
      };
      caller;
      nonces = ?nonces;
    })) {
      case (#err e) return #err(#Verify e);
      case (#ok a)  a;
    };

    // `getText` does scope-fallback, so requesting `"name"` matches
    // either bare `name` or `sso:dfinity.org:name`.
    let ?name = II.getText(attrs, "name") else return #err(#NoName);
    if (not isAdmin(name)) {
      return #err(#NotAdmin { name; admins });
    };
    #ok name
  };

  // Cheap lookup against the `adminSessions` cache. Used by every
  // admin action *except* `establishAdminSession`.
  func requireAdminSession(caller : Principal) : Result.Result<Text, AdminError> {
    switch (Map.get(adminSessions, Principal.compare, caller)) {
      case (?(name, expiresAt)) {
        if (Time.now() <= expiresAt) #ok name
        else #err(#SessionExpired)
      };
      case null #err(#SessionExpired);
    }
  };

  func isAdmin(name : Text) : Bool {
    switch (Array.find<Text>(admins, func(a) = a == name)) {
      case (?_) true;
      case null false;
    }
  };

  /// One full SSO + attribute-bundle verification per admin session.
  /// On success, caches `(caller -> (name, expiresAt))` so that the
  /// rest of the admin actions in this session are just a Map lookup.
  /// The frontend calls this immediately after `signInAdmin`.
  public shared ({ caller }) func establishAdminSession() : async Result.Result<{ name : Text; expiresAt : Int }, AdminError> {
    switch (verifyAdminAttributes(caller)) {
      case (#err e) #err e;
      case (#ok name) {
        let expiresAt = Time.now() + adminSessionNs;
        Map.add(adminSessions, Principal.compare, caller, (name, expiresAt));
        #ok({ name; expiresAt })
      };
    }
  };

  /// Drops the cached admin session — the Authorization-tier bundle
  /// can no longer be replayed (it was already nonce-consumed) but
  /// this clears the cache too.
  public shared ({ caller }) func endAdminSession() : async () {
    Map.remove(adminSessions, Principal.compare, caller);
  };

  /// Admin-facing list — descending by score, with full upvote counts.
  public shared ({ caller }) func adminListIssues() : async Result.Result<[IssueForAdmin], AdminError> {
    switch (requireAdminSession(caller)) { case (#err e) return #err e; case _ {} };

    let active = Iter.toArray(
      Iter.filter<Issue>(Map.values(issues), func(i) = not i.deleted)
    );
    let sorted = Array.sort<Issue>(active, func(a, b) : Order.Order = Nat.compare(b.upvotes, a.upvotes));
    #ok(Array.map<Issue, IssueForAdmin>(sorted, func(i) {
      { id = i.id; body = i.body; upvotes = i.upvotes; response = i.response; createdAt = i.createdAt }
    }))
  };

  /// Soft-delete: the issue stays in storage (so we don't reuse its id
  /// or break replays in tests) but disappears from every list.
  public shared ({ caller }) func adminDeleteIssue(id : Nat) : async Result.Result<(), AdminError> {
    switch (requireAdminSession(caller)) { case (#err e) return #err e; case _ {} };
    let ?issue = Map.get(issues, Nat.compare, id) else return #err(#NotFound);
    Map.add(issues, Nat.compare, id, { issue with deleted = true });
    #ok()
  };

  /// Adds an admin response. From this point on the issue is visible to
  /// everyone *with* its score, and `toggleUpvote` rejects with
  /// `#VotesLocked`.
  public shared ({ caller }) func adminRespond(id : Nat, response : Text) : async Result.Result<(), AdminError> {
    switch (requireAdminSession(caller)) { case (#err e) return #err e; case _ {} };
    let trimmed = Text.trim(response, #char ' ');
    if (Text.size(trimmed) == 0) return #err(#Empty);

    let ?issue = Map.get(issues, Nat.compare, id) else return #err(#NotFound);
    Map.add(issues, Nat.compare, id, { issue with response = ?trimmed });
    #ok()
  };

  // ------------------------------------------------------- helpers --

  // Fisher-Yates seeded by `entropy`. Each draw consumes a few bytes;
  // 32 bytes (`Random.blob()`'s default) is plenty for short lists.
  // The exact statistical bias from `byte mod range` is fine for the
  // anti-bias goal here — we're not running a lottery.
  func shuffle(arr : [Issue], entropy : Blob) : [Issue] {
    let n = arr.size();
    if (n <= 1) return arr;
    let bytes = Blob.toArray(entropy);
    let work = Array.thaw<Issue>(arr);
    var i = n - 1 : Nat;
    var b = 0;
    while (i > 0) {
      let r = if (b < bytes.size()) {
        let v = Nat8.toNat(bytes[b]);
        b += 1;
        v % (i + 1)
      } else {
        // Out of bytes — fall back to a deterministic step. Lists this
        // long don't show up in practice; if they ever do, refresh the
        // entropy by calling `Random.blob()` again.
        i % (i + 1)
      };
      let tmp = work[i];
      work[i] := work[r];
      work[r] := tmp;
      i -= 1;
    };
    Array.freeze<Issue>(work)
  };
};
