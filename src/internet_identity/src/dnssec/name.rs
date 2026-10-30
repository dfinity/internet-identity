//! [`DnsName`]: an admitted DNS name in canonical wire form.

use std::cmp::Ordering;
use std::fmt;

/// Longest wire form of a name, root label included (RFC 1035 §3.1).
const MAX_WIRE_LEN: usize = 255;
/// Longest label (RFC 1035 §2.3.4 / §3.1).
const MAX_LABEL_LEN: usize = 63;
/// A length octet with either of its top two bits set is not an
/// ordinary label: `11` is a compression pointer (RFC 1035 §4.1.4),
/// `01` an extended label type (RFC 6891 §5), `10` reserved.
const LABEL_TYPE_MASK: u8 = 0xC0;

/// A DNS name whose wire form (RFC 1035 §3.1) is canonical per
/// RFC 4034 §6.2: uncompressed labels with every ASCII letter
/// lower-cased, terminated by the root label. Every label is at most
/// 63 octets and the whole name at most 255 octets. The root is the
/// single zero octet.
///
/// Equality and hashing are over the canonical bytes, so two case
/// variants of one name are one value. `Ord` is canonical DNS name
/// order (RFC 4034 §6.1): names are compared label by label from the
/// root outwards, each label as an octet string, an ancestor sorting
/// before its descendants.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DnsName {
    wire: Box<[u8]>,
}

/// Why a byte sequence or a dotted string is not a [`DnsName`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NameError {
    /// A length octet with the top two bits set (compression pointer
    /// or extended label type).
    UnsupportedLabelType,
    /// A label longer than 63 octets.
    LabelTooLong,
    /// A wire form longer than 255 octets.
    NameTooLong,
    /// The input ends before the root label.
    MissingRootLabel,
    /// Bytes follow the root label.
    TrailingBytes,
    /// Two consecutive dots, or a leading dot, in dotted text.
    EmptyLabel,
    /// Dotted text with a byte outside ASCII.
    NonAscii,
}

impl DnsName {
    /// Admit a wire-form name, lower-casing its labels.
    pub fn from_wire(wire: &[u8]) -> Result<Self, NameError> {
        let mut out = Vec::with_capacity(wire.len().min(MAX_WIRE_LEN));
        let mut i = 0;
        loop {
            let &len = wire.get(i).ok_or(NameError::MissingRootLabel)?;
            if len & LABEL_TYPE_MASK != 0 {
                return Err(NameError::UnsupportedLabelType);
            }
            out.push(len);
            i += 1;
            if len == 0 {
                break;
            }
            let label = wire
                .get(i..i + len as usize)
                .ok_or(NameError::MissingRootLabel)?;
            out.extend(label.iter().map(u8::to_ascii_lowercase));
            i += len as usize;
            if out.len() >= MAX_WIRE_LEN {
                return Err(NameError::NameTooLong);
            }
        }
        if i < wire.len() {
            return Err(NameError::TrailingBytes);
        }
        Ok(Self {
            wire: out.into_boxed_slice(),
        })
    }

    /// Admit a dotted ASCII name. A trailing dot is tolerated; `""`
    /// and `"."` are the root. Escapes (RFC 1035 §5.1) are not
    /// interpreted.
    pub fn from_text(text: &str) -> Result<Self, NameError> {
        if !text.is_ascii() {
            return Err(NameError::NonAscii);
        }
        let body = text.strip_suffix('.').unwrap_or(text);
        let mut wire = Vec::with_capacity(body.len() + 2);
        if !body.is_empty() {
            for label in body.split('.') {
                if label.is_empty() {
                    return Err(NameError::EmptyLabel);
                }
                if label.len() > MAX_LABEL_LEN {
                    return Err(NameError::LabelTooLong);
                }
                wire.push(label.len() as u8);
                wire.extend_from_slice(label.as_bytes());
            }
        }
        wire.push(0);
        Self::from_wire(&wire)
    }

    /// The canonical wire form.
    pub fn as_wire(&self) -> &[u8] {
        &self.wire
    }

    /// Number of labels, the root label excluded (root = 0).
    pub fn label_count(&self) -> usize {
        self.labels().count()
    }

    /// The name with its first label removed; `None` for the root.
    pub fn parent(&self) -> Option<DnsName> {
        let first_len = self.wire[0] as usize;
        if first_len == 0 {
            return None;
        }
        Some(Self {
            wire: self.wire[1 + first_len..].into(),
        })
    }

    /// Whether `self` is `other` or lies below it. Every name is a
    /// subdomain of the root, and a name is a subdomain of itself.
    pub fn is_subdomain_of(&self, other: &DnsName) -> bool {
        self.label_starts().any(|i| self.wire[i..] == *other.wire)
    }

    /// Heap bytes this name retains.
    pub fn retained_bytes(&self) -> usize {
        self.wire.len()
    }

    /// Offsets of every length octet, the root label's included.
    fn label_starts(&self) -> impl Iterator<Item = usize> + '_ {
        let mut i = Some(0);
        std::iter::from_fn(move || {
            let start = i?;
            i = match self.wire[start] {
                0 => None,
                len => Some(start + 1 + len as usize),
            };
            Some(start)
        })
    }

    /// The labels in wire order (first label first), root excluded.
    fn labels(&self) -> impl Iterator<Item = &[u8]> {
        self.label_starts()
            .filter(|&i| self.wire[i] != 0)
            .map(|i| &self.wire[i + 1..i + 1 + self.wire[i] as usize])
    }
}

impl Ord for DnsName {
    fn cmp(&self, other: &Self) -> Ordering {
        let mine: Vec<&[u8]> = self.labels().collect();
        let theirs: Vec<&[u8]> = other.labels().collect();
        mine.iter().rev().cmp(theirs.iter().rev())
    }
}

impl PartialOrd for DnsName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Dotted lower-case text without the trailing dot; the root is `.`.
/// Octets that are not printable ASCII, and dots and backslashes
/// inside a label, are written as RFC 1035 §5.1 escapes.
impl fmt::Display for DnsName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.wire.len() == 1 {
            return f.write_str(".");
        }
        for (n, label) in self.labels().enumerate() {
            if n > 0 {
                f.write_str(".")?;
            }
            for &b in label {
                match b {
                    b'.' | b'\\' => write!(f, "\\{}", b as char)?,
                    0x21..=0x7E => write!(f, "{}", b as char)?,
                    _ => write!(f, "\\{b:03}")?,
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn name(text: &str) -> DnsName {
        DnsName::from_text(text).unwrap()
    }

    #[test]
    fn wire_round_trip_lowercases_labels() {
        let n = DnsName::from_wire(b"\x07Example\x03COM\x00").unwrap();
        assert_eq!(n.as_wire(), b"\x07example\x03com\x00");
        assert_eq!(n.to_string(), "example.com");
    }

    #[test]
    fn text_round_trip_lowercases_and_tolerates_trailing_dot() {
        assert_eq!(
            name("Selector1._domainkey.GMAIL.com").as_wire(),
            b"\x09selector1\x0a_domainkey\x05gmail\x03com\x00"
        );
        assert_eq!(name("example.com."), name("EXAMPLE.COM"));
        assert_eq!(name("example.com").to_string(), "example.com");
    }

    #[test]
    fn root_forms() {
        let root = DnsName::from_wire(b"\x00").unwrap();
        assert_eq!(name(""), root);
        assert_eq!(name("."), root);
        assert_eq!(root.as_wire(), b"\x00");
        assert_eq!(root.to_string(), ".");
        assert_eq!(root.label_count(), 0);
        assert_eq!(root.parent(), None);
        assert_eq!(root.retained_bytes(), 1);
    }

    #[test]
    fn wire_rejects_compression_pointer_and_extended_label_type() {
        assert_eq!(
            DnsName::from_wire(b"\xc0\x0c"),
            Err(NameError::UnsupportedLabelType)
        );
        assert_eq!(
            DnsName::from_wire(b"\x03com\xc0\x0c"),
            Err(NameError::UnsupportedLabelType)
        );
        assert_eq!(
            DnsName::from_wire(b"\x41abc\x00"),
            Err(NameError::UnsupportedLabelType)
        );
    }

    #[test]
    fn wire_rejects_missing_root_label() {
        assert_eq!(DnsName::from_wire(b""), Err(NameError::MissingRootLabel));
        assert_eq!(
            DnsName::from_wire(b"\x03com"),
            Err(NameError::MissingRootLabel)
        );
        assert_eq!(
            DnsName::from_wire(b"\x09example"),
            Err(NameError::MissingRootLabel)
        );
    }

    #[test]
    fn wire_rejects_trailing_bytes() {
        assert_eq!(
            DnsName::from_wire(b"\x03com\x00\x00"),
            Err(NameError::TrailingBytes)
        );
    }

    #[test]
    fn wire_rejects_name_over_255_octets() {
        // Four 63-octet labels: 4 × 64 + 1 = 257 octets.
        let mut wire = Vec::new();
        for _ in 0..4 {
            wire.push(63);
            wire.extend_from_slice(&[b'a'; 63]);
        }
        wire.push(0);
        assert_eq!(DnsName::from_wire(&wire), Err(NameError::NameTooLong));
        // Three 63-octet labels plus a 61-octet label: 255 exactly.
        let mut wire = Vec::new();
        for _ in 0..3 {
            wire.push(63);
            wire.extend_from_slice(&[b'a'; 63]);
        }
        wire.push(61);
        wire.extend_from_slice(&[b'a'; 61]);
        wire.push(0);
        assert_eq!(wire.len(), 255);
        assert!(DnsName::from_wire(&wire).is_ok());
    }

    #[test]
    fn text_rejects_empty_label() {
        assert_eq!(DnsName::from_text("a..b"), Err(NameError::EmptyLabel));
        assert_eq!(DnsName::from_text(".a"), Err(NameError::EmptyLabel));
        assert_eq!(DnsName::from_text("a.."), Err(NameError::EmptyLabel));
        assert_eq!(DnsName::from_text(".."), Err(NameError::EmptyLabel));
    }

    #[test]
    fn text_rejects_label_over_63_octets() {
        let label = "a".repeat(64);
        assert_eq!(
            DnsName::from_text(&format!("{label}.example.com")),
            Err(NameError::LabelTooLong)
        );
        let label = "a".repeat(63);
        assert!(DnsName::from_text(&format!("{label}.example.com")).is_ok());
    }

    #[test]
    fn text_rejects_name_over_255_octets() {
        let label = "a".repeat(50);
        let text = [label.as_str(); 5].join(".");
        assert_eq!(DnsName::from_text(&text), Err(NameError::NameTooLong));
    }

    #[test]
    fn text_rejects_non_ascii() {
        assert_eq!(
            DnsName::from_text("bücher.example"),
            Err(NameError::NonAscii)
        );
    }

    #[test]
    fn label_count_excludes_root() {
        assert_eq!(name("com").label_count(), 1);
        assert_eq!(name("example.com").label_count(), 2);
        assert_eq!(name("a.b.example.com.").label_count(), 4);
    }

    #[test]
    fn parent_drops_the_first_label() {
        let n = name("sub.example.com");
        assert_eq!(n.parent(), Some(name("example.com")));
        assert_eq!(name("com").parent(), Some(name(".")));
        assert_eq!(name("com").parent().unwrap().parent(), None);
    }

    #[test]
    fn is_subdomain_of_respects_label_boundaries() {
        assert!(name("example.com").is_subdomain_of(&name("com")));
        assert!(name("sub.example.com").is_subdomain_of(&name("example.com")));
        assert!(!name("evilexample.com").is_subdomain_of(&name("example.com")));
        assert!(!name("unrelated.org").is_subdomain_of(&name("example.com")));
        assert!(!name("com").is_subdomain_of(&name("example.com")));
    }

    #[test]
    fn is_subdomain_of_counts_equality_and_the_root() {
        assert!(name("com").is_subdomain_of(&name("com")));
        assert!(name("example.com").is_subdomain_of(&name(".")));
        assert!(name(".").is_subdomain_of(&name(".")));
        assert!(!name(".").is_subdomain_of(&name("com")));
        assert!(name("Example.COM").is_subdomain_of(&name("example.com")));
    }

    #[test]
    fn ordering_matches_rfc_4034_section_6_1_examples() {
        // RFC 4034 §6.1 lists these names in canonical order. The
        // non-ASCII and wildcard examples are built from wire form.
        let sorted = [
            name("example"),
            name("a.example"),
            name("yljkjljk.a.example"),
            name("Z.a.example"),
            name("zABC.a.EXAMPLE"),
            name("z.example"),
            DnsName::from_wire(b"\x01\x01\x01z\x07example\x00").unwrap(),
            name("*.z.example"),
            DnsName::from_wire(b"\x01\xc8\x01z\x07example\x00").unwrap(),
        ];
        let mut shuffled = sorted.to_vec();
        shuffled.reverse();
        shuffled.swap(1, 5);
        shuffled.sort();
        assert_eq!(shuffled, sorted);
        assert!(name(".") < name("example"));
        assert_eq!(name("a.example").cmp(&name("A.EXAMPLE")), Ordering::Equal);
    }

    #[test]
    fn retained_bytes_is_the_wire_length() {
        assert_eq!(name("example.com").retained_bytes(), 13);
    }

    #[test]
    fn display_escapes_unprintable_octets_and_inner_dots() {
        let n = DnsName::from_wire(b"\x03a.b\x01\x01\x07example\x00").unwrap();
        assert_eq!(n.to_string(), "a\\.b.\\001.example");
    }
}
