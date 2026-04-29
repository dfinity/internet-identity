import { useState } from "react";
import { useNavigate } from "react-router-dom";

import { signInAnonymous } from "../lib/auth";
import { sessionStore } from "../lib/sessionStore";

export function Home() {
  const navigate = useNavigate();
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const onSignIn = async () => {
    setError(null);
    setBusy(true);
    try {
      const s = await signInAnonymous();
      sessionStore.set(s);
      navigate("/issues");
    } catch (e) {
      setError(String(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <section className="card">
      <h1>Dfinsight</h1>
      <p className="lede">
        Anonymously share and upvote the matters of interest you'd like DFINITY
        to address.
      </p>
      <button onClick={onSignIn} disabled={busy} className="primary">
        {busy ? "Signing in…" : "Sign in anonymously"}
      </button>
      {error && <p className="error">{error}</p>}
    </section>
  );
}
