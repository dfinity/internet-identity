import type { EmailRecoveryDiagnostics } from "$lib/generated/internet_identity_types";
import { VERSION } from "$lib/legacy/environment";

/**
 * Build a compact, copyable, strictly-public diagnostics blob for a
 * failed email-recovery attempt.
 *
 * The canister-sourced fields come from `EmailRecoveryDiagnostics`,
 * which is curated to be non-sensitive (no email address, anchor,
 * principal, or inner error string — `reason_code` is the failing
 * variant's name only). The only FE-added field is the build id. The
 * result is therefore safe to paste into a support ticket, and the
 * `message-id` lets support line the case up across the SMTP gateway
 * and canister logs.
 *
 * `diag` is `undefined` when the failure is purely FE-side (e.g. the
 * delegation retrieval threw) — there is no canister challenge to read,
 * so only the build id is included.
 */
export const buildDiagnosticsBlob = (
  diag: EmailRecoveryDiagnostics | undefined,
): string => {
  const path =
    diag === undefined
      ? "—"
      : "Dnssec" in diag.verification_path
        ? "dnssec"
        : "doh";
  // `created_at` is nanoseconds since the epoch (candid nat64 → bigint).
  // Divide as bigint (exact ms) before narrowing to Number for `Date`.
  const time =
    diag !== undefined
      ? new Date(Number(diag.created_at / BigInt(1_000_000))).toISOString()
      : "—";
  // `VERSION` is `import.meta.env.II_VERSION` (untyped) and defaults to
  // "" when unset; narrow + check explicitly so it's a real string.
  const build =
    typeof VERSION === "string" && VERSION.length > 0 ? VERSION : "unknown";
  return [
    "Internet Identity — email recovery diagnostics",
    `build:      ${build}`,
    `message-id: ${diag?.message_id[0] ?? "—"}`,
    `reason:     ${diag?.reason_code ?? "—"}`,
    `path:       ${path}`,
    `time:       ${time}`,
  ].join("\n");
};
