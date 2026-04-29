import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";

import { sessionStore } from "../lib/sessionStore";
import { restoreAnonSession, signOut } from "../lib/auth";
import type {
  IssueForUser,
  PostError,
} from "../lib/declarations/dfinsight_backend.types";

function formatPostError(e: PostError): string {
  if ("NotSignedIn" in e) return "You need to sign in.";
  if ("Empty" in e) return "Write something first.";
  if ("TooLong" in e)
    return `Too long — keep it under ${e.TooLong.maxChars} characters.`;
  if ("DailyLimit" in e) {
    const ns = Number(e.DailyLimit.nextAllowedNs);
    const at = new Date(ns / 1_000_000);
    return `One issue per 24h. You can post again at ${at.toLocaleString()}.`;
  }
  return "Unknown error.";
}

export function Issues() {
  const navigate = useNavigate();
  const [session, setSession] = useState(sessionStore.get());
  const [issues, setIssues] = useState<IssueForUser[]>([]);
  const [body, setBody] = useState("");
  const [posting, setPosting] = useState(false);
  const [postError, setPostError] = useState<string | null>(null);
  const [canPost, setCanPost] = useState<{
    allowed: boolean;
    nextAllowedAt: Date | null;
  }>({ allowed: true, nextAllowedAt: null });

  useEffect(() => {
    if (session) return;
    // Came back to /issues after a reload — try restoring the cached
    // delegation from IndexedDB before bouncing the user to /.
    restoreAnonSession().then((s) => {
      if (s) {
        sessionStore.set(s);
        setSession(s);
      } else {
        navigate("/");
      }
    });
  }, [session, navigate]);

  useEffect(() => {
    if (!session) return;
    void refresh();
  }, [session]);

  async function refresh() {
    if (!session) return;
    const [list, status] = await Promise.all([
      session.backend.listIssuesForUser(),
      session.backend.myPostStatus(),
    ]);
    setIssues(list);
    setCanPost({
      allowed: status.allowed,
      nextAllowedAt:
        status.nextAllowedNs.length === 1
          ? new Date(Number(status.nextAllowedNs[0]) / 1_000_000)
          : null,
    });
  }

  async function onPost() {
    if (!session) return;
    setPostError(null);
    setPosting(true);
    try {
      const res = await session.backend.createIssue(body);
      if ("err" in res) {
        setPostError(formatPostError(res.err));
        return;
      }
      setBody("");
      await refresh();
    } finally {
      setPosting(false);
    }
  }

  async function onUpvote(id: bigint) {
    if (!session) return;
    const res = await session.backend.toggleUpvote(id);
    if ("err" in res) {
      // Optimistic refresh covers Deleted / VotesLocked / NotFound —
      // whatever changed under us, the next list will reflect it.
      await refresh();
      return;
    }
    setIssues((prev) =>
      prev.map((i) => (i.id === id ? { ...i, upvoted: res.ok.upvoted } : i)),
    );
  }

  async function onSignOut() {
    await signOut();
    sessionStore.set(null);
    navigate("/");
  }

  if (!session) return <p>Loading…</p>;

  return (
    <section className="card">
      <header className="row">
        <h1>Common matters of interest</h1>
        <button className="ghost" onClick={onSignOut}>
          Sign out
        </button>
      </header>

      <form
        className="post-form"
        onSubmit={(e) => {
          e.preventDefault();
          void onPost();
        }}
      >
        <textarea
          placeholder={
            canPost.allowed
              ? "Share a matter of interest…"
              : `Next post at ${canPost.nextAllowedAt?.toLocaleString() ?? "…"}`
          }
          maxLength={280}
          rows={3}
          value={body}
          onChange={(e) => setBody(e.target.value)}
          disabled={!canPost.allowed || posting}
        />
        <div className="row">
          <span className="hint">{body.length}/280</span>
          <button
            type="submit"
            disabled={!canPost.allowed || posting || body.trim().length === 0}
            className="primary"
          >
            {posting ? "Posting…" : "Post"}
          </button>
        </div>
        {postError && <p className="error">{postError}</p>}
      </form>

      <ul className="issues">
        {issues.length === 0 && <li className="empty">Nothing yet.</li>}
        {issues.map((i) => (
          <IssueRow key={String(i.id)} issue={i} onUpvote={onUpvote} />
        ))}
      </ul>
    </section>
  );
}

function IssueRow({
  issue,
  onUpvote,
}: {
  issue: IssueForUser;
  onUpvote: (id: bigint) => void;
}) {
  return (
    <li className={`issue ${issue.votesLocked ? "responded" : ""}`}>
      <p className="body">{issue.body}</p>
      {issue.response.length === 1 && (
        <blockquote className="response">
          <strong>Response:</strong> {issue.response[0]}
        </blockquote>
      )}
      <div className="row">
        <button
          className={issue.upvoted ? "voted" : ""}
          onClick={() => onUpvote(issue.id)}
          disabled={issue.votesLocked}
          title={
            issue.votesLocked
              ? "Voting closed — admin has responded"
              : issue.upvoted
                ? "You upvoted this"
                : "Upvote"
          }
        >
          {issue.upvoted ? "▲ Upvoted" : "△ Upvote"}
        </button>
        {issue.upvotes.length === 1 && (
          <span className="score">{String(issue.upvotes[0])} upvotes</span>
        )}
      </div>
    </li>
  );
}
