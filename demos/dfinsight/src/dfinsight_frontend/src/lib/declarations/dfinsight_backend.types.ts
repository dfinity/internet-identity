import type { ActorMethod } from "@icp-sdk/core/agent";

export type IIError =
  | { NoAttributes: null }
  | { MalformedCandid: null }
  | { MissingField: string }
  | { OriginMismatch: { expected: string; got: string } }
  | { Stale: { ageNs: bigint } }
  | { UnknownNonce: null }
  | { NonceExpired: null }
  | { MissingNonceStore: null };

export type PostError =
  | { NotSignedIn: null }
  | { Empty: null }
  | { TooLong: { maxChars: bigint } }
  | { DailyLimit: { nextAllowedNs: bigint } };

export type VoteError =
  | { NotSignedIn: null }
  | { NotFound: null }
  | { Deleted: null }
  | { VotesLocked: null };

export type AdminError =
  | { Verify: IIError }
  | { NoName: null }
  | { NotAdmin: { name: string; admins: string[] } }
  | { NotFound: null }
  | { Empty: null }
  | { SessionExpired: null };

export type Result<T, E> = { ok: T } | { err: E };

export interface IssueForUser {
  id: bigint;
  body: string;
  upvoted: boolean;
  response: [] | [string];
  upvotes: [] | [bigint];
  votesLocked: boolean;
}

export interface IssueForAdmin {
  id: bigint;
  body: string;
  upvotes: bigint;
  response: [] | [string];
  createdAt: bigint;
}

export interface PostStatus {
  allowed: boolean;
  nextAllowedNs: [] | [bigint];
}

export interface DfinsightBackend {
  generate_nonce: ActorMethod<[], Uint8Array | number[]>;
  createIssue: ActorMethod<[string], Result<bigint, PostError>>;
  toggleUpvote: ActorMethod<[bigint], Result<{ upvoted: boolean }, VoteError>>;
  listIssuesForUser: ActorMethod<[], IssueForUser[]>;
  myPostStatus: ActorMethod<[], PostStatus>;
  listAdmins: ActorMethod<[], string[]>;
  establishAdminSession: ActorMethod<
    [],
    Result<{ name: string; expiresAt: bigint }, AdminError>
  >;
  endAdminSession: ActorMethod<[], void>;
  adminListIssues: ActorMethod<[], Result<IssueForAdmin[], AdminError>>;
  adminDeleteIssue: ActorMethod<[bigint], Result<null, AdminError>>;
  adminRespond: ActorMethod<[bigint, string], Result<null, AdminError>>;
}
