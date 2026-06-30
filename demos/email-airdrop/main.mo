import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Char "mo:base/Char";
import Nat "mo:base/Nat";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Int "mo:base/Int";
import Blob "mo:base/Blob";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Buffer "mo:base/Buffer";
import Time "mo:base/Time";
import Map "mo:base/OrderedMap";
import Deque "mo:base/Deque";
import Timer "mo:base/Timer";
import Error "mo:base/Error";

// =============================================================================
// Email airdrop demo
//
// Sends 1 ICP to each user who emails airdrop@<your-domain> with:
//   1. their ICP address (64-hex account identifier) in the Subject,
//   2. at least one Cc with a friend's address,
//   3. a body containing the app link and the participation phrase (checked as
//      plain substrings, since mail clients append signatures/footers).
//
// Canisters cannot receive SMTP. The intended wiring is:
//   MX record -> inbound-email provider (SendGrid Inbound Parse / Mailgun
//   Routes / Cloudflare Email Worker) -> HTTP POST of the raw message to this
//   canister's http_request_update endpoint (guarded by a shared secret).
// `submitEmail` offers the same path as a typed call for local testing.
// See README.md for the full design, assumptions and threat model.
// =============================================================================

persistent actor Airdrop {

  // ----------------------------- Configuration -----------------------------
  // Edit these for your deployment (in production, prefer init args / a
  // controller-only `configure` method over baked-in constants).

  // ICP ledger canister id (mainnet). For local tests, deploy a ledger and
  // replace this with its id.
  let LEDGER_ID = "ryjl3-tyaaa-aaaaa-aaaba-cai";
  // Address shown on the landing page (where users send their email).
  let AIRDROP_EMAIL = "airdrop@CANISTER_DOMAIN";
  // The app URL that must appear (as a plain substring) in the email body.
  let APP_URL = "https://CANISTER_DOMAIN";
  // A short phrase that must appear (substring) in the email body.
  let REQUIRED_BODY_PHRASE = "I want to participate in the ICP airdrop";
  // Shared secret guarding the webhook and submitEmail entrypoints.
  // CHANGE THIS before deploying.
  let WEBHOOK_SECRET = "change-me-please";
  // Reward amount and ledger fee, in e8s (1 ICP = 100_000_000 e8s).
  let REWARD_E8S : Nat64 = 100_000_000;
  let FEE_E8S : Nat64 = 10_000;
  // How often (seconds) to retry the waitlist, e.g. after a top-up.
  let WAITLIST_INTERVAL_SECONDS : Nat = 300;

  // ------------------------------ Ledger types ------------------------------
  // ICP ledger: legacy `transfer` (to an account identifier) + ICRC-1 balance.

  type Tokens = { e8s : Nat64 };
  type TimeStamp = { timestamp_nanos : Nat64 };
  type TransferArgs = {
    memo : Nat64;
    amount : Tokens;
    fee : Tokens;
    from_subaccount : ?Blob;
    to : Blob; // 32-byte account identifier
    created_at_time : ?TimeStamp;
  };
  type TransferError = {
    #BadFee : { expected_fee : Tokens };
    #InsufficientFunds : { balance : Tokens };
    #TxTooOld : { allowed_window_nanos : Nat64 };
    #TxCreatedInFuture;
    #TxDuplicate : { duplicate_of : Nat64 };
  };
  type TransferResult = { #Ok : Nat64; #Err : TransferError };
  type Account = { owner : Principal; subaccount : ?Blob };
  type Ledger = actor {
    transfer : shared TransferArgs -> async TransferResult;
    icrc1_balance_of : shared query Account -> async Nat;
  };
  let ledger : Ledger = actor (LEDGER_ID);

  // ------------------------------ Domain types ------------------------------

  // A parsed inbound email.
  type Email = {
    from : Text;
    to : Text;
    cc : [Text];
    subject : Text;
    body : Text;
    dkimPassed : Bool; // verification result from the email provider
  };

  // Everyone who ever attempted to participate is stored here, keyed by the
  // (lower-cased) sender address: email_address -> participation record.
  type Participant = {
    email : Text;
    attempts : Nat;
    lastAttemptAt : Int;
    lastRewardedAt : ?Int; // null until the (one and only) reward
    rewardedAddress : ?Text;
    lastAddress : ?Text;
    onWaitlist : Bool;
    lastOutcome : Text;
  };

  // A queued reward, paid out (FIFO) once the canister is topped up.
  type Pending = {
    email : Text;
    address : Text;
    addressBytes : Blob;
    enqueuedAt : Int;
  };

  type Outcome = {
    #Rewarded : { blockIndex : Nat64; address : Text };
    #Waitlisted : { address : Text };
    #AlreadyParticipated;
    #Rejected : { reason : Text };
    #Unauthorized;
    #TransferFailed : { error : Text };
  };

  // --------------------------------- State ----------------------------------

  transient let emails = Map.Make<Text>(Text.compare);
  var participants : Map.Map<Text, Participant> = emails.empty<Participant>();

  var waitlist : Deque.Deque<Pending> = Deque.empty<Pending>();
  var waitlistLen : Nat = 0;

  var rewardedCount : Nat = 0;
  var totalRewardedE8s : Nat = 0;

  transient var waitlistTimer : ?Timer.TimerId = null;
  transient var draining : Bool = false;

  // ---------------------------- Small text utils ----------------------------

  func toLowerAscii(t : Text) : Text {
    Text.map(
      t,
      func(c : Char) : Char {
        if (c >= 'A' and c <= 'Z') { Char.fromNat32(Char.toNat32(c) + 32) } else { c };
      },
    );
  };

  func isHexByte(b : Nat8) : Bool {
    (b >= 0x30 and b <= 0x39) // 0-9
    or (b >= 0x61 and b <= 0x66) // a-f
    or (b >= 0x41 and b <= 0x46) // A-F
  };

  func hexVal(b : Nat8) : ?Nat {
    if (b >= 0x30 and b <= 0x39) { ?(Nat8.toNat(b) - 0x30) } else if (b >= 0x61 and b <= 0x66) {
      ?(Nat8.toNat(b) - 0x61 + 10);
    } else if (b >= 0x41 and b <= 0x46) { ?(Nat8.toNat(b) - 0x41 + 10) } else {
      null;
    };
  };

  func toHex(bytes : [Nat8]) : Text {
    let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"];
    var s = "";
    for (b in bytes.vals()) {
      let v = Nat8.toNat(b);
      s #= digits[v / 16] # digits[v % 16];
    };
    s;
  };

  func jsonStr(s : Text) : Text {
    let escaped = Text.replace(Text.replace(s, #char '\\', "\\\\"), #char '\"', "\\\"");
    "\"" # escaped # "\"";
  };

  // ------------------------- ICP address validation -------------------------

  // CRC32 (IEEE 802.3), used by ICP account identifiers as a 4-byte checksum.
  func crc32(data : [Nat8]) : Nat32 {
    var crc : Nat32 = 0xFFFFFFFF;
    for (b in data.vals()) {
      crc := crc ^ Nat32.fromNat(Nat8.toNat(b));
      var k = 0;
      while (k < 8) {
        let mask : Nat32 = if (crc & 1 == 1) { 0xEDB88320 } else { 0 };
        crc := (crc >> 1) ^ mask;
        k += 1;
      };
    };
    crc ^ 0xFFFFFFFF;
  };

  // An account identifier is 32 bytes: a 4-byte big-endian CRC32 of the
  // following 28 bytes. Validate the checksum so we only accept well-formed
  // addresses.
  func validAccountId(b : [Nat8]) : Bool {
    if (b.size() != 32) { return false };
    let payload = Array.tabulate<Nat8>(28, func(i) = b[i + 4]);
    let actual = crc32(payload);
    let n32 = func(x : Nat8) : Nat32 = Nat32.fromNat(Nat8.toNat(x));
    let expected = (n32(b[0]) << 24) | (n32(b[1]) << 16) | (n32(b[2]) << 8) | n32(b[3]);
    actual == expected;
  };

  func decodeAccountId(hex64 : [Nat8]) : ?[Nat8] {
    let out = Buffer.Buffer<Nat8>(32);
    var k = 0;
    while (k < 64) {
      switch (hexVal(hex64[k]), hexVal(hex64[k + 1])) {
        case (?hi, ?lo) { out.add(Nat8.fromNat(hi * 16 + lo)) };
        case _ { return null };
      };
      k += 2;
    };
    let bytes = Buffer.toArray(out);
    if (validAccountId(bytes)) { ?bytes } else { null };
  };

  // Scans the subject for a 64-hex token and returns the decoded 32 bytes of
  // the first one whose checksum is valid. Assumes the address is a whitespace-
  // or punctuation-delimited token (the usual case).
  func findIcpAddress(subject : Text) : ?[Nat8] {
    let bytes = Blob.toArray(Text.encodeUtf8(subject));
    let n = bytes.size();
    let run = Buffer.Buffer<Nat8>(64);
    var i = 0;
    while (i <= n) {
      let isHex = (i < n) and isHexByte(bytes[i]);
      if (isHex) {
        run.add(bytes[i]);
      } else {
        if (run.size() >= 64) {
          let arr = Buffer.toArray(run);
          let first64 = Array.tabulate<Nat8>(64, func(j) = arr[j]);
          switch (decodeAccountId(first64)) {
            case (?valid) { return ?valid };
            case null {};
          };
        };
        run.clear();
      };
      i += 1;
    };
    null;
  };

  // ------------------------- Eligibility predicates -------------------------

  func isEmailLike(t : Text) : Bool {
    Text.contains(t, #char '@') and Text.contains(t, #char '.') and Text.size(t) >= 5;
  };

  // At least one Cc that is a valid email address and is not the sender.
  func ccHasFriend(cc : [Text], fromKey : Text) : Bool {
    for (c in cc.vals()) {
      let addr = toLowerAscii(Text.trim(c, #char ' '));
      if (isEmailLike(addr) and addr != fromKey) { return true };
    };
    false;
  };

  // Body must contain the app link and the participation phrase as substrings.
  func bodyOk(body : Text) : Bool {
    Text.contains(body, #text APP_URL) and Text.contains(body, #text REQUIRED_BODY_PHRASE);
  };

  // ----------------------------- Participant ops ----------------------------

  func defaultParticipant(key : Text) : Participant {
    {
      email = key;
      attempts = 0;
      lastAttemptAt = 0;
      lastRewardedAt = null;
      rewardedAddress = null;
      lastAddress = null;
      onWaitlist = false;
      lastOutcome = "";
    };
  };

  func updateParticipant(key : Text, f : Participant -> Participant) {
    let cur = switch (emails.get(participants, key)) {
      case (?p) { p };
      case null { defaultParticipant(key) };
    };
    participants := emails.put(participants, key, f(cur));
  };

  func recordAttempt(key : Text) {
    updateParticipant(
      key,
      func(p) = { p with attempts = p.attempts + 1; lastAttemptAt = Time.now(); lastOutcome = "received" },
    );
  };

  func setOutcome(key : Text, outcome : Text) {
    updateParticipant(key, func(p) = { p with lastOutcome = outcome });
  };

  func markRewarded(key : Text, address : Text, block : Nat64) {
    updateParticipant(
      key,
      func(p) = {
        p with
        lastRewardedAt = ?Time.now();
        rewardedAddress = ?address;
        lastAddress = ?address;
        onWaitlist = false;
        lastOutcome = "rewarded (block " # Nat64.toText(block) # ")";
      },
    );
    rewardedCount += 1;
    totalRewardedE8s += Nat64.toNat(REWARD_E8S);
  };

  func setWaitlisted(key : Text, address : Text) {
    updateParticipant(
      key,
      func(p) = { p with onWaitlist = true; lastAddress = ?address; lastOutcome = "waitlisted" },
    );
  };

  func clearWaitFlag(key : Text) {
    updateParticipant(key, func(p) = { p with onWaitlist = false });
  };

  func enqueue(key : Text, address : Text, addressBytes : Blob) {
    waitlist := Deque.pushBack(waitlist, { email = key; address; addressBytes; enqueuedAt = Time.now() });
    waitlistLen += 1;
    setWaitlisted(key, address);
  };

  // ------------------------------- Transfers --------------------------------

  func sendReward(to : Blob) : async { #ok : Nat64; #insufficient; #failed : Text } {
    let args : TransferArgs = {
      memo = 0;
      amount = { e8s = REWARD_E8S };
      fee = { e8s = FEE_E8S };
      from_subaccount = null;
      to;
      created_at_time = null;
    };
    try {
      switch (await ledger.transfer(args)) {
        case (#Ok b) { #ok b };
        case (#Err(#InsufficientFunds _)) { #insufficient };
        case (#Err other) { #failed(transferErrText(other)) };
      };
    } catch (e) {
      #failed(Error.message(e));
    };
  };

  func transferErrText(e : TransferError) : Text {
    switch e {
      case (#BadFee _) { "BadFee" };
      case (#InsufficientFunds _) { "InsufficientFunds" };
      case (#TxTooOld _) { "TxTooOld" };
      case (#TxCreatedInFuture) { "TxCreatedInFuture" };
      case (#TxDuplicate _) { "TxDuplicate" };
    };
  };

  // -------------------------------- Ingestion -------------------------------

  func ingest(email : Email) : async Outcome {
    let key = toLowerAscii(Text.trim(email.from, #char ' '));
    if (key == "") { return #Rejected { reason = "Missing From address." } };

    // Save every attempt (requirement: store all who attempted).
    recordAttempt(key);

    // One participation per email address.
    switch (emails.get(participants, key)) {
      case (?p) {
        if (p.lastRewardedAt != null) { return #AlreadyParticipated };
        if (p.onWaitlist) {
          return #Waitlisted { address = optText(p.lastAddress) };
        };
      };
      case null {};
    };

    // Rule 1: a valid ICP address in the subject.
    switch (findIcpAddress(email.subject)) {
      case null {
        setOutcome(key, "rejected:no-address");
        return #Rejected { reason = "No valid ICP address (a 64-hex account identifier) found in the Subject." };
      };
      case (?addrBytes) {
        // Rule 2: at least one friend in Cc.
        if (not ccHasFriend(email.cc, key)) {
          setOutcome(key, "rejected:no-cc");
          return #Rejected { reason = "Cc at least one friend (a valid email address different from yours)." };
        };
        // Rule 3: body contains the app link and the participation phrase.
        if (not bodyOk(email.body)) {
          setOutcome(key, "rejected:bad-body");
          return #Rejected { reason = "The body must contain the app link (" # APP_URL # ") and the phrase \"" # REQUIRED_BODY_PHRASE # "\"." };
        };

        // Eligible. Pay out while funds last; otherwise join the waitlist.
        let addrHex = toHex(addrBytes);
        let toBlob = Blob.fromArray(addrBytes);
        switch (await sendReward(toBlob)) {
          case (#ok b) {
            markRewarded(key, addrHex, b);
            #Rewarded { blockIndex = b; address = addrHex };
          };
          case (#insufficient) {
            enqueue(key, addrHex, toBlob);
            #Waitlisted { address = addrHex };
          };
          case (#failed msg) {
            setOutcome(key, "transfer-failed: " # msg);
            #TransferFailed { error = msg };
          };
        };
      };
    };
  };

  func optText(t : ?Text) : Text {
    switch t { case (?x) { x }; case null { "" } };
  };

  // ------------------------------- Waitlist ---------------------------------

  // Drains the waitlist (FIFO) while funds last. Safe to call repeatedly; the
  // `draining` guard prevents overlap with the timer.
  func processWaitlist() : async () {
    if (draining) { return };
    draining := true;
    label drain loop {
      switch (Deque.popFront(waitlist)) {
        case null { break drain };
        case (?(p, rest)) {
          switch (await sendReward(p.addressBytes)) {
            case (#ok b) {
              waitlist := rest;
              waitlistLen -= 1;
              markRewarded(p.email, p.address, b);
            };
            case (#insufficient) {
              // Funds ran out again; keep this entry at the front and stop.
              waitlist := Deque.pushFront(rest, p);
              break drain;
            };
            case (#failed msg) {
              // Drop a permanently-failing entry so it can't block the queue.
              waitlist := rest;
              waitlistLen -= 1;
              clearWaitFlag(p.email);
              setOutcome(p.email, "transfer-failed: " # msg);
            };
          };
        };
      };
    };
    draining := false;
  };

  func startTimer<system>() {
    switch (waitlistTimer) {
      case (?_) {};
      case null {
        waitlistTimer := ?Timer.recurringTimer<system>(
          #seconds WAITLIST_INTERVAL_SECONDS,
          func() : async () { await processWaitlist() },
        );
      };
    };
  };

  // ------------------------------ Public API --------------------------------

  // Typed ingestion entrypoint (used for local testing). Same secret as the
  // webhook so the eligibility/funds logic can be exercised without standing
  // up real mail infrastructure.
  public func submitEmail(secret : Text, email : Email) : async Outcome {
    if (secret != WEBHOOK_SECRET) { return #Unauthorized };
    await ingest(email);
  };

  // Manually drain the waitlist (e.g. right after a top-up).
  public func triggerWaitlist(secret : Text) : async () {
    if (secret != WEBHOOK_SECRET) { return };
    await processWaitlist();
  };

  type Stats = {
    participants : Nat;
    rewarded : Nat;
    waitlist : Nat;
    totalRewardedE8s : Nat;
    balanceE8s : Nat;
    rewardE8s : Nat;
    airdropEmail : Text;
  };

  public func getStats() : async Stats {
    let balance = try {
      await ledger.icrc1_balance_of({ owner = Principal.fromActor(Airdrop); subaccount = null });
    } catch (_) { 0 };
    {
      participants = emails.size(participants);
      rewarded = rewardedCount;
      waitlist = waitlistLen;
      totalRewardedE8s = totalRewardedE8s;
      balanceE8s = balance;
      rewardE8s = Nat64.toNat(REWARD_E8S);
      airdropEmail = AIRDROP_EMAIL;
    };
  };

  // Look up a single participant by email address.
  public query func getParticipant(email : Text) : async ?Participant {
    emails.get(participants, toLowerAscii(Text.trim(email, #char ' ')));
  };

  public query func getRules() : async Text { rulesText() };

  // --------------------------- HTTP gateway -----------------------------
  // GET  /            -> the rules landing page
  // GET  /rules.txt   -> the rules as plain text
  // POST /?secret=... -> upgraded to http_request_update (the email webhook)

  type HeaderField = (Text, Text);
  type HttpRequest = {
    method : Text;
    url : Text;
    headers : [HeaderField];
    body : Blob;
  };
  type HttpResponse = {
    status_code : Nat16;
    headers : [HeaderField];
    body : Blob;
    upgrade : ?Bool;
  };

  func resp(status : Nat16, contentType : Text, body : Text, upgrade : ?Bool) : HttpResponse {
    {
      status_code = status;
      headers = [("content-type", contentType)];
      body = Text.encodeUtf8(body);
      upgrade;
    };
  };

  func pathOf(url : Text) : Text {
    let parts = Iter.toArray(Text.split(url, #char '?'));
    if (parts.size() > 0) { parts[0] } else { url };
  };

  public query func http_request(req : HttpRequest) : async HttpResponse {
    let method = toLowerAscii(req.method);
    if (method == "post") {
      // Force re-issue as an update call so we can mutate state.
      return { status_code = 200; headers = []; body = Text.encodeUtf8(""); upgrade = ?true };
    };
    let path = pathOf(req.url);
    if (path == "/rules.txt") {
      return resp(200, "text/plain; charset=utf-8", rulesText(), null);
    };
    resp(200, "text/html; charset=utf-8", landingHtml(), null);
  };

  public func http_request_update(req : HttpRequest) : async HttpResponse {
    let method = toLowerAscii(req.method);
    if (method != "post") {
      return resp(405, "text/plain; charset=utf-8", "Method Not Allowed", null);
    };
    // Authenticate the webhook with the shared secret in the query string.
    if (not Text.contains(req.url, #text("secret=" # WEBHOOK_SECRET))) {
      return resp(401, "application/json", "{\"error\":\"unauthorized\"}", null);
    };
    let bodyText = switch (Text.decodeUtf8(req.body)) {
      case (?t) { t };
      case null { return resp(400, "application/json", "{\"error\":\"bad body encoding\"}", null) };
    };
    switch (parseRawEmail(bodyText)) {
      case null {
        resp(400, "application/json", "{\"error\":\"could not parse email\"}", null);
      };
      case (?email) {
        let outcome = await ingest(email);
        resp(200, "application/json", outcomeToJson(outcome), null);
      };
    };
  };

  func outcomeToJson(o : Outcome) : Text {
    switch o {
      case (#Rewarded r) {
        "{\"status\":\"rewarded\",\"block\":" # Nat64.toText(r.blockIndex) # ",\"address\":" # jsonStr(r.address) # "}";
      };
      case (#Waitlisted w) {
        "{\"status\":\"waitlisted\",\"address\":" # jsonStr(w.address) # "}";
      };
      case (#AlreadyParticipated) { "{\"status\":\"already_participated\"}" };
      case (#Rejected r) {
        "{\"status\":\"rejected\",\"reason\":" # jsonStr(r.reason) # "}";
      };
      case (#Unauthorized) { "{\"status\":\"unauthorized\"}" };
      case (#TransferFailed f) {
        "{\"status\":\"transfer_failed\",\"error\":" # jsonStr(f.error) # "}";
      };
    };
  };

  // ----------------------- Raw email (RFC822-lite) --------------------------
  // Parses `Key: value` headers, a blank line, then the body. This is the shape
  // an inbound-email provider delivers (raw MIME headers + body). We only read
  // From / To / Cc / Subject and a DKIM/Authentication-Results pass signal.

  func dropFirst(a : [Text]) : Iter.Iter<Text> {
    if (a.size() <= 1) { return Iter.fromArray<Text>([]) };
    Iter.fromArray(Array.tabulate<Text>(a.size() - 1, func(i) = a[i + 1]));
  };

  // "Name <a@b.com>" -> "a@b.com"; otherwise returns the trimmed input.
  func stripAngle(t : Text) : Text {
    if (Text.contains(t, #char '<')) {
      let a = Iter.toArray(Text.split(t, #char '<'));
      if (a.size() >= 2) {
        let b = Iter.toArray(Text.split(a[1], #char '>'));
        if (b.size() >= 1) { return Text.trim(b[0], #char ' ') };
      };
    };
    Text.trim(t, #char ' ');
  };

  func parseRawEmail(raw : Text) : ?Email {
    let lines = Iter.toArray(Text.split(raw, #char '\n'));
    var from = "";
    var to = "";
    var subject = "";
    var dkim = false;
    let ccs = Buffer.Buffer<Text>(2);
    let bodyLines = Buffer.Buffer<Text>(8);
    var inBody = false;

    for (rawLine in lines.vals()) {
      let line = Text.trimEnd(rawLine, #char '\r');
      if (inBody) {
        bodyLines.add(line);
      } else if (line == "") {
        inBody := true;
      } else {
        let parts = Iter.toArray(Text.split(line, #char ':'));
        if (parts.size() >= 2) {
          let key = toLowerAscii(Text.trim(parts[0], #char ' '));
          let value = Text.trim(Text.join(":", dropFirst(parts)), #char ' ');
          if (key == "from") { from := stripAngle(value) } else if (key == "to") {
            to := stripAngle(value);
          } else if (key == "cc") {
            for (a in Text.split(value, #char ',')) {
              ccs.add(stripAngle(Text.trim(a, #char ' ')));
            };
          } else if (key == "subject") { subject := value } else if (
            key == "dkim" or key == "x-dkim" or key == "authentication-results"
          ) {
            if (Text.contains(toLowerAscii(value), #text "pass")) { dkim := true };
          };
        };
      };
    };

    if (from == "" and subject == "") { return null };
    ?{
      from;
      to;
      cc = Buffer.toArray(ccs);
      subject;
      body = Text.join("\n", bodyLines.vals());
      dkimPassed = dkim;
    };
  };

  // ------------------------------- Content ----------------------------------

  func rulesText() : Text {
    "ICP Email Airdrop — Rules\n" #
    "=========================\n\n" #
    "Send an email to " # AIRDROP_EMAIL # " to receive 1 ICP.\n\n" #
    "To be eligible, your email must:\n" #
    "  1. Put your ICP address (64-hex account identifier) in the SUBJECT.\n" #
    "  2. Cc at least one friend (any valid email address that is not yours).\n" #
    "  3. Include in the BODY both:\n" #
    "       - the app link: " # APP_URL # "\n" #
    "       - the phrase:   \"" # REQUIRED_BODY_PHRASE # "\"\n\n" #
    "Notes:\n" #
    "  - One airdrop per email address, ever.\n" #
    "  - Rewards are paid while the canister has funds. If it is empty you join\n" #
    "    the waitlist (FIFO) and get paid automatically when it is topped up.\n";
  };

  func landingHtml() : Text {
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">" #
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">" #
    "<title>ICP Email Airdrop</title><style>" #
    "body{font-family:system-ui,sans-serif;max-width:680px;margin:3rem auto;padding:0 1rem;line-height:1.6;color:#1a1a2e}" #
    "h1{color:#3b00b9}code{background:#f0f0f5;padding:.1rem .35rem;border-radius:4px}" #
    "ol{padding-left:1.2rem}li{margin:.5rem 0}.card{background:#f7f7fb;border:1px solid #e3e3ef;border-radius:12px;padding:1rem 1.25rem;margin:1.5rem 0}" #
    ".muted{color:#666;font-size:.9rem}</style></head><body>" #
    "<h1>🪙 ICP Email Airdrop</h1>" #
    "<p>Send an email to <code>" # AIRDROP_EMAIL # "</code> and receive <strong>1 ICP</strong>.</p>" #
    "<div class=\"card\"><h2>How to participate</h2><ol>" #
    "<li>Put your <strong>ICP address</strong> (64-hex account identifier) in the <strong>Subject</strong>.</li>" #
    "<li><strong>Cc at least one friend</strong> (any valid email address that isn't yours).</li>" #
    "<li>In the <strong>body</strong>, include both the app link <code>" # APP_URL # "</code> " #
    "and the phrase <code>" # REQUIRED_BODY_PHRASE # "</code>.</li>" #
    "</ol></div>" #
    "<div class=\"card\"><h2>The fine print</h2><ul>" #
    "<li><strong>One airdrop per email address</strong>, ever.</li>" #
    "<li>Rewards are paid <strong>while the canister has funds</strong>.</li>" #
    "<li>If the canister is empty you join the <strong>waitlist</strong> and are paid " #
    "automatically (in order) when it is topped up.</li>" #
    "</ul></div>" #
    "<p class=\"muted\">This is a demo. Email From addresses are spoofable; in production " #
    "rewards should require a DKIM-verified sender.</p>" #
    "</body></html>";
  };

  // ------------------------------- Lifecycle --------------------------------

  startTimer<system>();

  system func postupgrade() { startTimer<system>() };
};
