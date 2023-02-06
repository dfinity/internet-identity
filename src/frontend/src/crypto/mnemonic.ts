import { entropyToMnemonic, wordlists, validateMnemonic } from "bip39";
import { toHexString } from "@dfinity/identity/lib/cjs/buffer";
import { isUserNumber } from "@utils/userNumber";

/**
 * @returns A random BIP39 mnemonic with 256 bits of entropy (generates a list of 24 words)
 */
export function generate(): string {
  const entropy = new Uint32Array(8); // NOTE: please change RECOVERYPHRASE_WORDCOUNT if this changes
  crypto.getRandomValues(entropy);
  return entropyToMnemonic(
    toHexString(entropy.buffer),
    RECOVERYPHRASE_WORDLIST
  );
}

/** How many words are expected in the recovery phrase */
export const RECOVERYPHRASE_WORDCOUNT = 24;

/** The list of words used to make the recovery phrase */
// NOTE: "english" is the only one actually bundled in (see webpack config)
export const RECOVERYPHRASE_WORDLIST: string[] = wordlists.english;

/**
 * @returns true if the mnemonic is valid, false otherwise.
 */
export function validate(mnemonic: string): boolean {
  return validateMnemonic(mnemonic);
}

/** A recovery phrase that was parsed.
 * An object of this type should not be used to reconstruct a recovery phrase, since
 * some information (though most likely incorrectly inserted) like extra whitespaces
 * is lost.
 */
type ParsedRecoveryPhrase = {
  leadingUserNumber: string | null;
  words: string[];
};

/** Parse recovery phrase into a structured object */
export const parseRecoveryPhrase = (s: string): ParsedRecoveryPhrase => {
  /* Split nicely into words
   * ' 2002 foo  bar ' -> split(" ") -> [ '', '2002', 'foo', '', 'bar', '' ]
   * [ '', '2002', 'foo', '', 'bar', '' ] -> filter(...) -> [ '2002', 'foo', 'bar' ]
   */
  const words = s.split(" ").filter((w) => w !== "");

  // check if there's a leading user (anchor) number
  const leadingUserNumber: string | null =
    words.length >= 1 && isUserNumber(words[0]) ? words[0] : null;

  // If the first element is indeed a number (BigInt), drop it from the actual mnemonic
  if (leadingUserNumber !== null) {
    words.shift();
  }

  return { leadingUserNumber, words };
};

/** Drop the anchor number from a string
 * Example: " 10005 foo bar" -> "foo bar"
 */
export const dropLeadingUserNumber = (input: string): string => {
  const parsed = parseRecoveryPhrase(input);
  return parsed.leadingUserNumber !== null
    ? input.replace(parsed.leadingUserNumber, "").trim()
    : input;
};

/** The warning types that may be generated. For more information on each, see 'recoveryPhraseWarnings'.
 *
 * We generate the messages independently mostly so that we can programatically test the warnings generated
 * (and avoid brittle regex-based testing).*/
export type Warning =
  | { type: "bad_chars"; chars: string[] }
  | { type: "repeated_whitespace"; between: [string, string] | null }
  | { type: "bad_anchor"; anchor: bigint }
  | { type: "bad_word_count"; count: number }
  | { type: "bad_words"; words: string[] }
  | { type: "invalid" };

/** Generates warnings for a given recovery phrase and user number */
export const recoveryPhraseWarnings: {
  (userNumber: bigint, inputRaw: string): Warning | null;
}[] = [
  // The input contains some characters that are not expected in a recovery phrase
  (userNumber: bigint, inputRaw: string): Warning | null => {
    // Check for anything that's not a letter, a number or a whitespace (' ', not \n)
    const badChars = inputRaw.match(/[^a-z0-9\s]|[\n\t\r]/g);

    if (badChars) {
      return { type: "bad_chars", chars: [...new Set(badChars)] };
    }

    return null;
  },

  // Check for double (or triple) whitespaces between words
  (userNumber: bigint, inputRaw: string): Warning | null => {
    const repeatedSpace = inputRaw.match(/(\S+)\s\s+(\S+)/);
    if (repeatedSpace) {
      let between: [string, string] | null = null;
      if (repeatedSpace.length === 3) {
        between = [repeatedSpace[1], repeatedSpace[2]];
      }

      return { type: "repeated_whitespace", between };
    }
    return null;
  },

  // Check for an incorrect anchor number (e.g. "10005 foo bar" when recovery anchor 10004)
  (userNumber: bigint, inputRaw: string): Warning | null => {
    const input = parseRecoveryPhrase(inputRaw);
    const leadingUserNumber =
      input.leadingUserNumber !== null ? BigInt(input.leadingUserNumber) : null;

    if (leadingUserNumber !== null && leadingUserNumber !== userNumber) {
      return { type: "bad_anchor", anchor: leadingUserNumber };
    }

    return null;
  },

  // We expect a specific word count
  (userNumber: bigint, inputRaw: string): Warning | null => {
    const input = parseRecoveryPhrase(inputRaw);

    if (
      input.words.length >= 1 &&
      input.words.length !== RECOVERYPHRASE_WORDCOUNT
    ) {
      return { type: "bad_word_count", count: input.words.length };
    }

    return null;
  },

  // As for characters, check for any words that we do not expect
  (userNumber: bigint, inputRaw: string): Warning | null => {
    const input = parseRecoveryPhrase(inputRaw);

    const badWords = [];
    for (const word of input.words) {
      if (!RECOVERYPHRASE_WORDLIST.includes(word)) {
        badWords.push(word);
      }
    }

    if (badWords.length >= 1) {
      return { type: "bad_words", words: [...new Set(badWords)] };
    }

    return null;
  },

  // We expect a specific word count
  (userNumber: bigint, inputRaw: string): Warning | null => {
    const input = parseRecoveryPhrase(inputRaw);

    if (
      input.words.length === RECOVERYPHRASE_WORDCOUNT &&
      !validate(dropLeadingUserNumber(inputRaw))
    ) {
      return { type: "invalid" };
    }

    return null;
  },
];

export const getWarnings = (userNumber: bigint, input: string): Warning[] => {
  const warnings = [];
  for (const mkWarning of recoveryPhraseWarnings) {
    const warning = mkWarning(userNumber, input);
    if (warning) {
      warnings.push(warning);
    }
  }
  return warnings;
};
