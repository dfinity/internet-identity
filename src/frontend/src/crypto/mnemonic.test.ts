import {
  Warning,
  parseRecoveryPhrase,
  getWarnings,
  dropLeadingUserNumber,
} from "./mnemonic";

const expectLeadingUserNumberToBe = (
  received: string,
  expected: number | null
) => {
  const parsed = parseRecoveryPhrase(received);
  const lun = parsed.leadingUserNumber
    ? Number(parsed.leadingUserNumber)
    : null;
  expect(lun).toBe(expected);
};

test("Leading user number is parsed correctly", () => {
  expectLeadingUserNumberToBe("10043", 10043);
  expectLeadingUserNumberToBe(" 10043 ", 10043);
  expectLeadingUserNumberToBe(" 10043 foo bar", 10043);
  expectLeadingUserNumberToBe("   10043 foo  bar", 10043);
  expectLeadingUserNumberToBe("10043n foo  bar", null);
  expectLeadingUserNumberToBe("foo 123 bar", null);
  expectLeadingUserNumberToBe("", null);
});

test("Leading user number is dropped correctly", () => {
  expect(dropLeadingUserNumber("10043")).toEqual("");
  expect(dropLeadingUserNumber("10043 ")).toEqual("");
  expect(dropLeadingUserNumber("10043 foo bar")).toEqual("foo bar");
  expect(dropLeadingUserNumber("10043  foo    bar")).toEqual("foo    bar");
  expect(dropLeadingUserNumber("foo    bar")).toEqual("foo    bar");
});

const expectWordsToEqual = (received: string, expected: string[]) => {
  expect(parseRecoveryPhrase(received).words).toEqual(expected);
};

test("Word list is parsed correctly", () => {
  expectWordsToEqual("about abandon", ["about", "abandon"]);
  expectWordsToEqual(" about   abandon     ", ["about", "abandon"]);
  expectWordsToEqual("", []);
  expectWordsToEqual("about about about abandon 123", [
    "about",
    "about",
    "about",
    "abandon",
    "123",
  ]);
});

const expectWarningsInclude = (
  userNumber: number,
  input: string,
  warning: Warning
) => {
  expect(getWarnings(BigInt(userNumber), input)).toEqual(
    expect.arrayContaining([warning])
  );
};

const expectNoWarnings = (userNumber: number, input: string) => {
  expect(getWarnings(BigInt(userNumber), input)).toEqual([]);
};

test("Warnings are correctly generated", () => {
  expectWarningsInclude(10001, "squirrel &", {
    type: "bad_chars",
    chars: ["&"],
  });

  expectWarningsInclude(10001, "about squirrel  bar abandon", {
    type: "repeated_whitespace",
    between: ["squirrel", "bar"],
  });

  expectWarningsInclude(10001, "10042 about about", {
    type: "bad_anchor",
    anchor: BigInt(10042),
  });

  expectWarningsInclude(10001, Array(23).fill("about").join(" "), {
    type: "bad_word_count",
    count: 23,
  });

  expectWarningsInclude(10001, `10042 ${Array(23).fill("about").join(" ")}`, {
    type: "bad_word_count",
    count: 23,
  });

  expectWarningsInclude(10001, "about abut squirrel squirel abandon squirel", {
    type: "bad_words",
    words: ["abut", "squirel"],
  });

  expectWarningsInclude(10001, Array(24).fill("about").join(" "), {
    type: "invalid",
  });

  expectNoWarnings(
    10001,
    "10001 trust dad oak bright arrow pipe omit provide material donkey hole worry parade test paper fix clutch state range census dust fan hurry almost"
  );
});
