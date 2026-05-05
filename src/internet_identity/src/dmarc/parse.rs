//! DMARC DNS record parser per RFC 7489 §6.3.
//!
//! A DMARC record is a TXT RR at `_dmarc.<domain>` with the same
//! tag-value list shape as DKIM (a `;`-separated `k=v` sequence). The
//! verifier only honours the tags listed here; reporting/forensic tags
//! (`fo=` / `rua=` / `ruf=` / `rf=`) are accepted at parse time and
//! dropped — we don't act on policy *for* the sending domain, only
//! against it.

use super::types::{AlignmentMode, DmarcPolicy, DmarcRecord};

/// Parse a DMARC TXT record. Returns `Err` with a human-readable
/// reason on malformed input; the caller maps that to
/// [`DmarcOutcome::Malformed`].
pub fn parse_dmarc_txt(record: &str) -> Result<DmarcRecord, String> {
    let tags = split_tag_list(record)?;

    // §6.3: v=DMARC1 MUST be the first tag. Any other order is invalid.
    let v = tags
        .first()
        .ok_or_else(|| "empty DMARC record".to_string())?;
    if !v.0.eq_ignore_ascii_case("v") {
        return Err("v= must be the first tag".to_string());
    }
    if !v.1.eq_ignore_ascii_case("DMARC1") {
        return Err(format!("unsupported DMARC version v={}", v.1));
    }

    let get = |name: &str| -> Option<&str> {
        tags.iter()
            .find(|(k, _)| k.eq_ignore_ascii_case(name))
            .map(|(_, v)| v.as_str())
    };

    // §6.3: p= is required, must be one of {none, quarantine, reject}.
    let policy = match get("p")
        .ok_or_else(|| "missing required p= tag".to_string())?
        .trim()
        .to_ascii_lowercase()
        .as_str()
    {
        "none" => DmarcPolicy::None,
        "quarantine" => DmarcPolicy::Quarantine,
        "reject" => DmarcPolicy::Reject,
        other => return Err(format!("unsupported p={other}")),
    };

    // sp= inherits from p= when absent or unrecognised. Per §6.3 the
    // tag MUST be one of the three policy values when present.
    let subdomain_policy = match get("sp") {
        None => policy,
        Some(v) => match v.trim().to_ascii_lowercase().as_str() {
            "none" => DmarcPolicy::None,
            "quarantine" => DmarcPolicy::Quarantine,
            "reject" => DmarcPolicy::Reject,
            other => return Err(format!("unsupported sp={other}")),
        },
    };

    // adkim/aspf default to relaxed.
    let adkim = match get("adkim") {
        None => AlignmentMode::Relaxed,
        Some(v) => parse_alignment_mode(v, "adkim")?,
    };
    let aspf = match get("aspf") {
        None => AlignmentMode::Relaxed,
        Some(v) => parse_alignment_mode(v, "aspf")?,
    };

    // pct= defaults to 100. Only u8 0..=100 is valid per §6.3.
    let pct = match get("pct") {
        None => 100u8,
        Some(v) => {
            let n: u32 = v
                .trim()
                .parse()
                .map_err(|_| format!("pct= is not a number: {v}"))?;
            if n > 100 {
                return Err(format!("pct={n} out of range 0..=100"));
            }
            n as u8
        }
    };

    Ok(DmarcRecord {
        policy,
        subdomain_policy,
        adkim,
        aspf,
        pct,
    })
}

fn parse_alignment_mode(v: &str, label: &str) -> Result<AlignmentMode, String> {
    match v.trim().to_ascii_lowercase().as_str() {
        "s" => Ok(AlignmentMode::Strict),
        "r" => Ok(AlignmentMode::Relaxed),
        other => Err(format!("unsupported {label}={other}")),
    }
}

/// Split a tag-value list into `(name, value)` pairs in receipt order.
///
/// Like the DKIM tag-list splitter ([`crate::dkim::parse`]) but
/// preserving order so we can enforce "v= must be first" per §6.3.
/// Per RFC 7489 §6.3 each tag may appear at most once; we reject
/// duplicates so a malformed-but-still-parseable record can't smuggle
/// surprising semantics past us.
fn split_tag_list(record: &str) -> Result<Vec<(String, String)>, String> {
    let mut tags: Vec<(String, String)> = Vec::new();
    for part in record.split(';') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        let (name, value) = match part.split_once('=') {
            Some(p) => p,
            None => return Err(format!("tag without '=' separator: {part}")),
        };
        let name = name.trim().to_string();
        if name.is_empty() {
            return Err("empty tag name".into());
        }
        if tags
            .iter()
            .any(|(existing, _)| existing.eq_ignore_ascii_case(&name))
        {
            return Err(format!("duplicate tag {name}"));
        }
        tags.push((name, value.trim().to_string()));
    }
    Ok(tags)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_minimal_record() {
        let r = "v=DMARC1; p=none";
        let parsed = parse_dmarc_txt(r).unwrap();
        assert_eq!(parsed.policy, DmarcPolicy::None);
        assert_eq!(parsed.subdomain_policy, DmarcPolicy::None);
        assert_eq!(parsed.adkim, AlignmentMode::Relaxed);
        assert_eq!(parsed.aspf, AlignmentMode::Relaxed);
        assert_eq!(parsed.pct, 100);
    }

    #[test]
    fn parses_full_record() {
        let r = "v=DMARC1; p=reject; sp=quarantine; adkim=s; aspf=s; pct=50; \
                 fo=1; rua=mailto:reports@example.com";
        let parsed = parse_dmarc_txt(r).unwrap();
        assert_eq!(parsed.policy, DmarcPolicy::Reject);
        assert_eq!(parsed.subdomain_policy, DmarcPolicy::Quarantine);
        assert_eq!(parsed.adkim, AlignmentMode::Strict);
        assert_eq!(parsed.aspf, AlignmentMode::Strict);
        assert_eq!(parsed.pct, 50);
    }

    #[test]
    fn case_insensitive_tag_names_and_values() {
        let r = "V=DMARC1; P=Reject; ADKIM=S";
        let parsed = parse_dmarc_txt(r).unwrap();
        assert_eq!(parsed.policy, DmarcPolicy::Reject);
        assert_eq!(parsed.adkim, AlignmentMode::Strict);
    }

    #[test]
    fn sp_inherits_from_p_when_absent() {
        let r = "v=DMARC1; p=quarantine";
        let parsed = parse_dmarc_txt(r).unwrap();
        assert_eq!(parsed.subdomain_policy, DmarcPolicy::Quarantine);
    }

    #[test]
    fn rejects_v_not_first() {
        let r = "p=none; v=DMARC1";
        let err = parse_dmarc_txt(r).unwrap_err();
        assert!(err.contains("v="));
    }

    #[test]
    fn rejects_unsupported_v() {
        let r = "v=DMARC2; p=none";
        let err = parse_dmarc_txt(r).unwrap_err();
        assert!(err.contains("DMARC2"));
    }

    #[test]
    fn rejects_missing_p() {
        let r = "v=DMARC1; sp=none";
        let err = parse_dmarc_txt(r).unwrap_err();
        assert!(err.contains("p="));
    }

    #[test]
    fn rejects_unsupported_p() {
        let r = "v=DMARC1; p=warn";
        let err = parse_dmarc_txt(r).unwrap_err();
        assert!(err.contains("p=warn"));
    }

    #[test]
    fn rejects_pct_out_of_range() {
        let r = "v=DMARC1; p=none; pct=150";
        let err = parse_dmarc_txt(r).unwrap_err();
        assert!(err.contains("pct"));
    }

    #[test]
    fn rejects_duplicate_tag() {
        let r = "v=DMARC1; p=none; p=reject";
        let err = parse_dmarc_txt(r).unwrap_err();
        assert!(err.contains("duplicate"));
    }

    #[test]
    fn unknown_tags_are_ignored() {
        let r = "v=DMARC1; p=none; unknownnewtag=42; rua=mailto:foo@bar";
        let parsed = parse_dmarc_txt(r).unwrap();
        assert_eq!(parsed.policy, DmarcPolicy::None);
    }

    #[test]
    fn whitespace_around_separators_tolerated() {
        let r = "v=DMARC1 ; p = quarantine ;  adkim = r ";
        let parsed = parse_dmarc_txt(r).unwrap();
        assert_eq!(parsed.policy, DmarcPolicy::Quarantine);
        assert_eq!(parsed.adkim, AlignmentMode::Relaxed);
    }
}
