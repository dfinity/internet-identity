import { useEffect, useState } from "react";
import { Link, useNavigate } from "react-router-dom";

import { signInAdmin, makePublicBackend, AdminSignInError } from "../lib/auth";
import { sessionStore } from "../lib/sessionStore";
import type { AdminError } from "../lib/declarations/dfinsight_backend.types";

function formatAdminError(e: AdminError): string {
  if ("Verify" in e) {
    const tag = Object.keys(e.Verify)[0];
    return `Attribute verification failed (${tag}). Try signing in again.`;
  }
  if ("NoName" in e)
    return "Your SSO bundle didn't include a name. Make sure you grant the name attribute.";
  if ("NotAdmin" in e) {
    return [
      `You signed in as "${e.NotAdmin.name}", which is not on the admin list.`,
      `Current admins: ${e.NotAdmin.admins.join(", ")}.`,
    ].join("\n");
  }
  if ("SessionExpired" in e) return "Session expired — sign in again.";
  if ("NotFound" in e) return "Issue not found.";
  if ("Empty" in e) return "Response can't be empty.";
  return "Unknown error.";
}

export function AdminLanding() {
  const navigate = useNavigate();
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [admins, setAdmins] = useState<string[]>([]);

  useEffect(() => {
    void (async () => {
      const backend = await makePublicBackend();
      try {
        setAdmins(await backend.listAdmins());
      } catch {
        // Public read can fail if the canister id is wrong — let the
        // sign-in flow surface the real error.
      }
    })();
  }, []);

  const onSignIn = async () => {
    setError(null);
    setBusy(true);
    try {
      const s = await signInAdmin();
      sessionStore.set(s);
      navigate("/admin/panel");
    } catch (e) {
      if (e instanceof AdminSignInError) {
        setError(formatAdminError(e.adminError));
      } else {
        setError(String(e));
      }
    } finally {
      setBusy(false);
    }
  };

  return (
    <section className="card">
      <h1>Dfinsight — Admin</h1>
      <p className="lede">
        Admins can read every matter of interest with its upvote score, delete
        spam, and post a public response (which closes voting on that issue).
        Admins cannot post or upvote — that only happens from the user page.
      </p>
      <button onClick={onSignIn} disabled={busy} className="primary">
        {busy ? "Signing in…" : "Sign in as Dfinsight admin"}
      </button>

      <details className="admins">
        <summary>Current admins ({admins.length})</summary>
        <ul>
          {admins.map((name) => (
            <li key={name}>{name}</li>
          ))}
        </ul>
      </details>

      {error && <pre className="error">{error}</pre>}

      <p className="back-link">
        <Link to="/">← Back</Link>
      </p>

      <details className="info">
        <summary>How does this verify I'm an admin?</summary>
        <p>
          Sign-in opens id.ai with the DFINITY SSO 1-click flow and requests the
          verified <code>sso:dfinity.org:name</code> attribute. The backend
          canister reads it via the IC's <code>sender_info</code> mechanism
          (through <code>mo:identity-attributes</code>) and checks the name
          against the allowlist.
        </p>
      </details>
    </section>
  );
}
