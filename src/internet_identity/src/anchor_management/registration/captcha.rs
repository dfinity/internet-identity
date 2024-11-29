use crate::state::ChallengeInfo;
use crate::{random_salt, secs_to_nanos, state};
use captcha::fonts::Default as DefaultFont;
use captcha::fonts::Font;
use ic_cdk::api::time;
use ic_cdk::trap;
use internet_identity_interface::internet_identity::types::{ChallengeAttempt, ChallengeKey};
use lazy_static::lazy_static;
use rand_core::{RngCore, SeedableRng};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct Base64(pub String);

lazy_static! {
    /// Problematic characters that are easily mixed up by humans to "normalized" replacement.
    /// I.e. the captcha will only contain a "replaced" character (values below in map) if the
    /// character also appears as a "replacement" (keys below in map). All occurrences of
    /// "replaced" characters in the user's challenge result will be replaced with the
    /// "replacements".
    /// Note: the captcha library already excludes the characters o, O and 0.
    static ref CHAR_REPLACEMENTS: HashMap<char, Vec<char>> = vec![
        ('c', vec!['c', 'C']),
        ('i', vec!['1', 'i', 'l', 'I', 'j']),
        ('s', vec!['s', 'S']),
        ('x', vec!['x', 'X']),
        ('y', vec!['y', 'Y']),
        ('z', vec!['z', 'Z']),
        ('p', vec!['p', 'P']),
        ('w', vec!['w', 'W']),
    ].into_iter().collect();

    /// The font (glyphs) used when creating captchas
    static ref CAPTCHA_FONT: DefaultFont = DefaultFont::new();

    /// The character set used in CAPTCHA challenges (font charset with replacements)
    static ref CHALLENGE_CHARSET: Vec<char> = {
        // To get the final charset:
        // * Start with all chars supported by the font by default
        // * Remove all the chars that will be "replaced"
        // * Add (potentially re-add) replacement chars
        let mut chars = CAPTCHA_FONT.chars();
        {
          let dropped: HashSet<char> = CHAR_REPLACEMENTS.values().flat_map(|x| x.clone()).collect();
          chars.retain(|c| !dropped.contains(c));
        }

        {
          chars.append(&mut CHAR_REPLACEMENTS.keys().copied().collect());
        }

        chars
    };
}

/// Remove challenges older than CAPTCHA_CHALLENGE_LIFETIME from the inflight challenges map
pub fn prune_expired_challenges(inflight_challenges: &mut HashMap<ChallengeKey, ChallengeInfo>) {
    // 5 mins
    const CAPTCHA_CHALLENGE_LIFETIME_NS: u64 = secs_to_nanos(300);

    inflight_challenges.retain(|_, v| v.created > time() - CAPTCHA_CHALLENGE_LIFETIME_NS);
}

// Get a random number generator based on 'raw_rand'
pub async fn make_rng() -> rand_chacha::ChaCha20Rng {
    let seed = random_salt().await;
    rand_chacha::ChaCha20Rng::from_seed(seed)
}

// Generate an n-char long string of random characters. The characters are sampled from the rang
// a-z.
//
// NOTE: The 'rand' crate (currently) does not build on wasm32-unknown-unknown so we have to
// make-do with the RngCore trait (as opposed to Rng), therefore we have to implement this
// ourselves as opposed to using one of rand's distributions.
pub fn random_string<T: RngCore>(rng: &mut T, n: usize) -> String {
    let mut chars: Vec<u8> = vec![];

    // The range
    let a: u8 = b'a';
    let z: u8 = b'z';

    // n times, get a random number as u32, then shrink to u8, and finally shrink to the size of
    // our range. Finally, offset by the start of our range.
    for _ in 0..n {
        let next: u8 = rng.next_u32() as u8 % (z - a) + a;
        chars.push(next);
    }

    String::from_utf8_lossy(&chars).to_string()
}

#[cfg(feature = "dummy_captcha")]
pub fn create_captcha<T: RngCore>(rng: T) -> (Base64, String) {
    let mut captcha = captcha::new_captcha_with(rng, CAPTCHA_FONT.clone());
    let captcha = captcha.set_charset(&vec!['a']).add_chars(1).view(96, 48);

    let resp = match captcha.as_base64() {
        Some(png_base64) => Base64(png_base64),
        None => trap("Could not get base64 of captcha"),
    };

    return (resp, captcha.chars_as_string());
}

const CAPTCHA_LENGTH: usize = 5;

#[cfg(not(feature = "dummy_captcha"))]
pub fn create_captcha<T: RngCore>(rng: T) -> (Base64, String) {
    use captcha::filters::Wave;

    let mut captcha = captcha::new_captcha_with(rng, CAPTCHA_FONT.clone());

    let captcha = captcha
        // Replace the default charset with our more readable charset
        .set_charset(&CHALLENGE_CHARSET)
        // add some characters
        .add_chars(CAPTCHA_LENGTH as u32)
        .apply_filter(Wave::new(2.0, 20.0).horizontal())
        .apply_filter(Wave::new(2.0, 20.0).vertical())
        .view(220, 120);
    // if you ever change the size of the captcha, make sure to also change the
    // CSS in the frontend to match the new size (.c-captcha-placeholder)

    let resp = match captcha.as_base64() {
        Some(png_base64) => Base64(png_base64),
        None => trap("Could not get base64 of captcha"),
    };

    (resp, captcha.chars_as_string())
}

// Check whether the CAPTCHA challenge was solved
pub fn check_challenge(res: ChallengeAttempt) -> Result<(), ()> {
    let Some(challenge) = state::inflight_challenges_mut(|inflight_challenges| {
        prune_expired_challenges(inflight_challenges);
        inflight_challenges.remove(&res.key)
    }) else {
        return Err(());
    };

    check_captcha_solution(res.chars, challenge.chars)
}

/// Check whether the supplied CAPTCHA solution attempt matches the expected solution (after
/// normalizing ambiguous characters).
pub fn check_captcha_solution(solution_attempt: String, solution: String) -> Result<(), ()> {
    // avoid processing too many characters
    if solution_attempt.len() > CAPTCHA_LENGTH {
        return Err(());
    }
    // Normalize challenge attempts by replacing characters that are not in the captcha character set
    // with the respective replacement from CHAR_REPLACEMENTS.
    let normalized_solution_attempt: String = solution_attempt
        .chars()
        .map(|c| {
            // Apply all replacements
            *CHAR_REPLACEMENTS
                .iter()
                // For each key, see if the char matches any of the values (replaced chars) and if
                // so replace with the key itself (replacement char)
                .find_map(|(k, v)| if v.contains(&c) { Some(k) } else { None })
                .unwrap_or(&c)
        })
        .collect();

    if normalized_solution_attempt != solution {
        return Err(());
    }
    Ok(())
}
