//! What members are called. A name is claimed by a principal and is unique
//! among the names claimed: a second Alice is Alice_1, a third Alice_2.

/// Characters, not bytes. A longer name is cut, not refused.
pub const MAX_NAME_CHARS: usize = 32;

/// The name to give somebody who asked for `wanted`, given which names are
/// taken by others. The name as they wrote it where it is free, otherwise
/// the first of `wanted_1`, `wanted_2`, … that is. Names are compared
/// without regard to case, so one Alice cannot pass for another.
///
/// `None` where nothing is left once the whitespace around it goes.
pub fn assign(wanted: &str, taken_by_others: impl Fn(&str) -> bool) -> Option<String> {
    let base: String = wanted.trim().chars().take(MAX_NAME_CHARS).collect();
    let base = base.trim_end().to_string();
    if base.is_empty() {
        return None;
    }
    if !taken_by_others(&base) {
        return Some(base);
    }
    (1..)
        .map(|n| format!("{base}_{n}"))
        .find(|candidate| !taken_by_others(candidate))
}

/// Whether two names are the same name.
pub fn same(a: &str, b: &str) -> bool {
    a.to_lowercase() == b.to_lowercase()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn taken<'a>(names: &'a [&'a str]) -> impl Fn(&str) -> bool + 'a {
        move |candidate| names.iter().any(|name| same(name, candidate))
    }

    #[test]
    fn a_free_name_is_given_as_written() {
        assert_eq!(assign("Alice", taken(&[])), Some("Alice".to_string()));
        assert_eq!(
            assign("  Alice ", taken(&["Bob"])),
            Some("Alice".to_string()),
            "surrounding whitespace is not part of the name"
        );
    }

    #[test]
    fn a_taken_name_gets_the_first_free_subscript() {
        assert_eq!(
            assign("Alice", taken(&["Alice"])),
            Some("Alice_1".to_string())
        );
        assert_eq!(
            assign("Alice", taken(&["Alice", "Alice_1", "Alice_3"])),
            Some("Alice_2".to_string())
        );
    }

    #[test]
    fn case_does_not_tell_names_apart() {
        assert_eq!(
            assign("alice", taken(&["Alice"])),
            Some("alice_1".to_string())
        );
        assert!(same("Alice", "ALICE"));
        assert!(!same("Alice", "Alicia"));
    }

    #[test]
    fn nothing_is_not_a_name_and_a_long_one_is_cut() {
        assert_eq!(assign("   ", taken(&[])), None);
        let long = "x".repeat(MAX_NAME_CHARS + 5);
        assert_eq!(
            assign(&long, taken(&[])).map(|name| name.chars().count()),
            Some(MAX_NAME_CHARS)
        );
    }
}
