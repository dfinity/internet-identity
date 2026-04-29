// Hand-rolled IDL factory mirroring `src/dfinsight_backend/main.mo`.
// Regenerate from the canister's auto-emitted .did once `icp build`
// runs in CI; until then this is the source of truth for the frontend.
//
// The `idl` parameter shape matches the @dfinity-style factory contract:
// `Actor.createActor` injects the module-level `IDL` from
// `@icp-sdk/core/candid`, which includes both class constructors and
// helpers like `encode`/`decode`. Typed as `any` here because the
// `InterfaceFactory` type isn't re-exported from the package entry.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const idlFactory = ({ IDL }: { IDL: any }) => {
  const IIError = IDL.Variant({
    NoAttributes: IDL.Null,
    MalformedCandid: IDL.Null,
    MissingField: IDL.Text,
    OriginMismatch: IDL.Record({ expected: IDL.Text, got: IDL.Text }),
    Stale: IDL.Record({ ageNs: IDL.Nat }),
    UnknownNonce: IDL.Null,
    NonceExpired: IDL.Null,
    MissingNonceStore: IDL.Null,
  });

  const PostError = IDL.Variant({
    NotSignedIn: IDL.Null,
    Empty: IDL.Null,
    TooLong: IDL.Record({ maxChars: IDL.Nat }),
    DailyLimit: IDL.Record({ nextAllowedNs: IDL.Int }),
  });

  const VoteError = IDL.Variant({
    NotSignedIn: IDL.Null,
    NotFound: IDL.Null,
    Deleted: IDL.Null,
    VotesLocked: IDL.Null,
  });

  const AdminError = IDL.Variant({
    Verify: IIError,
    NoName: IDL.Null,
    NotAdmin: IDL.Record({ name: IDL.Text, admins: IDL.Vec(IDL.Text) }),
    NotFound: IDL.Null,
    Empty: IDL.Null,
    SessionExpired: IDL.Null,
  });

  const IssueForUser = IDL.Record({
    id: IDL.Nat,
    body: IDL.Text,
    upvoted: IDL.Bool,
    response: IDL.Opt(IDL.Text),
    upvotes: IDL.Opt(IDL.Nat),
    votesLocked: IDL.Bool,
  });

  const IssueForAdmin = IDL.Record({
    id: IDL.Nat,
    body: IDL.Text,
    upvotes: IDL.Nat,
    response: IDL.Opt(IDL.Text),
    createdAt: IDL.Int,
  });

  const PostStatus = IDL.Record({
    allowed: IDL.Bool,
    nextAllowedNs: IDL.Opt(IDL.Int),
  });

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const Result = (ok: any, err: any) => IDL.Variant({ ok, err });

  return IDL.Service({
    generate_nonce: IDL.Func([], [IDL.Vec(IDL.Nat8)], []),

    createIssue: IDL.Func([IDL.Text], [Result(IDL.Nat, PostError)], []),
    toggleUpvote: IDL.Func(
      [IDL.Nat],
      [Result(IDL.Record({ upvoted: IDL.Bool }), VoteError)],
      [],
    ),
    listIssuesForUser: IDL.Func([], [IDL.Vec(IssueForUser)], []),
    myPostStatus: IDL.Func([], [PostStatus], ["query"]),
    listAdmins: IDL.Func([], [IDL.Vec(IDL.Text)], ["query"]),

    establishAdminSession: IDL.Func(
      [],
      [
        Result(
          IDL.Record({ name: IDL.Text, expiresAt: IDL.Int }),
          AdminError,
        ),
      ],
      [],
    ),
    endAdminSession: IDL.Func([], [], []),
    adminListIssues: IDL.Func(
      [],
      [Result(IDL.Vec(IssueForAdmin), AdminError)],
      [],
    ),
    adminDeleteIssue: IDL.Func(
      [IDL.Nat],
      [Result(IDL.Null, AdminError)],
      [],
    ),
    adminRespond: IDL.Func(
      [IDL.Nat, IDL.Text],
      [Result(IDL.Null, AdminError)],
      [],
    ),
  });
};
