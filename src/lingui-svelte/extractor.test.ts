import { svelteExtractor } from "./extractor";
import { describe, expect } from "vitest";
import { ExtractedMessage } from "@lingui/conf";

const FILE_NAME = "test.svelte";

const extractAll = async (code: string): Promise<ExtractedMessage[]> => {
  const messages: ExtractedMessage[] = [];
  await svelteExtractor.extract(FILE_NAME, code, (message) =>
    messages.push(message),
  );
  return messages;
};

const extract = (code: string): Promise<ExtractedMessage> =>
  new Promise<ExtractedMessage>((resolve, reject) => {
    try {
      svelteExtractor.extract(FILE_NAME, code, resolve);
    } catch (err) {
      reject(err);
    }
  });

describe("svelteExtractor", () => {
  describe("tagged template", () => {
    it("should extract filename, line and column number", async () => {
      const { origin } = await extract("{$t`Hello World`}");
      expect(origin).toEqual([FILE_NAME, 1, 1]);
    });

    it.each([
      {
        case: "without variables",
        code: "{$t`Hello World`}",
        expected: "Hello World",
      },
      {
        case: "with named variable",
        code: "{$t`Hello ${name}`}",
        expected: "Hello {name}",
      },
      {
        case: "with positional variable",
        code: '{$t`Hello ${"John"}`}',
        expected: "Hello {0}",
      },
      {
        case: "with named and positional variables",
        code: '{$t`Hello ${name}, ${"John"}, ${friend} and ${"Jack"}`}',
        expected: "Hello {name}, {0}, {friend} and {1}",
      },
    ])("should extract message $case", async ({ code, expected }) => {
      const { message } = await extract(code);
      expect(message).toEqual(expected);
    });
  });

  describe("call expression", () => {
    it("should extract filename, line and column number", async () => {
      const { origin } = await extract('{$t({ message: "Hello World" })}');
      expect(origin).toEqual([FILE_NAME, 1, 1]);
    });

    it("should extract explicit id", async () => {
      const { id } = await extract(
        '{$t({ id: "HELLO_WORLD", message: "Hello World" })}',
      );
      expect(id).toEqual("HELLO_WORLD");
    });

    it("should extract optional context", async () => {
      const { context } = await extract(
        '{$t({ message: "Hello World", context: "Greeting the world" })}',
      );
      expect(context).toEqual("Greeting the world");
    });

    it.each([
      {
        case: "without variables",
        code: '{$t({ message: "Hello World" })}',
        expected: "Hello World",
      },
      {
        case: "with named variable",
        code: "{$t({ message: `Hello ${name}` })}",
        expected: "Hello {name}",
      },
      {
        case: "with positional variable",
        code: '{$t({ message: `Hello ${"John"}` })}',
        expected: "Hello {0}",
      },
      {
        case: "with named and positional variables",
        code: '{$t({ message: `Hello ${name}, ${"John"}, ${friend} and ${"Jack"}` })}',
        expected: "Hello {name}, {0}, {friend} and {1}",
      },
    ])("should extract message $case", async ({ code, expected }) => {
      const { message } = await extract(code);
      expect(message).toEqual(expected);
    });
  });

  describe("plural call expression", () => {
    it("should extract filename, line and column number", async () => {
      const { origin } = await extract(
        '{$plural(1, { one: "One book", other: "# Books" })}',
      );
      expect(origin).toEqual([FILE_NAME, 1, 1]);
    });

    it.each([
      {
        case: "with num value",
        code: '{$plural(1, { one: "One book", other: "# Books" })}',
        expected: "{num, plural, one {One book} other {# Books}}",
      },
      {
        case: "with num variable",
        code: '{$plural(numBooks, { one: "One book", other: "# Books" })}',
        expected: "{numBooks, plural, one {One book} other {# Books}}",
      },
      {
        case: "with exact plural",
        code: '{$plural(0, { one: "One book", other: "# Books", "=0": "No books" })}',
        expected: "{num, plural, one {One book} other {# Books} =0 {No books}}",
      },
      {
        case: "with named variable",
        code: "{$plural(1, { one: `One ${genre} book`, other: `# ${genre} books` })}",
        expected:
          "{num, plural, one {One {genre} book} other {# {genre} books}}",
      },
      {
        case: "with positional variable",
        code: '{$plural(1, { one: `One ${"fantasy"} book`, other: `# ${"fantasy"} books` })}',
        expected: "{num, plural, one {One {0} book} other {# {0} books}}",
      },
      {
        case: "with named and positional variables",
        code: '{$plural(1, { one: `One ${genre} and ${"fantasy"} book`, other: `# ${genre} and ${"fantasy"} books` })}',
        expected:
          "{num, plural, one {One {genre} and {0} book} other {# {genre} and {0} books}}",
      },
    ])("should extract message $case", async ({ code, expected }) => {
      const { message } = await extract(code);
      expect(message).toEqual(expected);
    });
  });

  describe("<Trans> component", () => {
    it("should extract filename, line and column number", async () => {
      const { origin } = await extract("<Trans>Hello world</Trans>");
      expect(origin).toEqual([FILE_NAME, 1, 0]);
    });

    it("should extract explicit id", async () => {
      const { id } = await extract(
        '<Trans id="HELLO_WORLD">Hello world</Trans>',
      );
      expect(id).toEqual("HELLO_WORLD");
    });

    it("should extract optional context", async () => {
      const { context } = await extract(
        '<Trans context="Greeting the world">Hello world</Trans>',
      );
      expect(context).toEqual("Greeting the world");
    });

    it.each([
      {
        case: "without variables",
        code: "<Trans>Hello world</Trans>",
        expected: "Hello world",
      },
      {
        case: "with named variable",
        code: "<Trans>Hello {name}</Trans>",
        expected: "Hello {name}",
      },
      {
        case: "with positional variable",
        code: '<Trans>Hello {"John"}</Trans>',
        expected: "Hello {0}",
      },
      {
        case: "with named and positional variables",
        code: '<Trans>Hello {name}, {"John"}, {friend} and {"Jack"}</Trans>',
        expected: "Hello {name}, {0}, {friend} and {1}",
      },
      {
        case: "with tag",
        code: '<Trans>Click <a href="/upgrade">here</a> to upgrade</Trans>',
        expected: "Click <0>here</0> to upgrade",
      },
      {
        case: "with self closing tag",
        code: "<Trans>Hello<br>World</Trans>",
        expected: "Hello<0/>World",
      },
      {
        case: "with nested tags",
        code: "<Trans>To continue, <strong>please <em>confirm</em></strong> your choice.</Trans>",
        expected: "To continue, <1>please <0>confirm</0></1> your choice.",
      },
      {
        case: "with tags and variables",
        code: '<Trans>Hi {name}, please <a href="/profile"><strong>update</strong> your profile</a> or <em>contact {"support"}</em> for help.<br/>Thank you!</Trans>',
        expected:
          "Hi {name}, please <1><0>update</0> your profile</1> or <2>contact {0}</2> for help.<3/>Thank you!",
      },
      {
        case: "with comments",
        code: '<Trans><!-- Comment -->Click <a href="/upgrade"><!-- Comment -->here<!-- Comment --></a> to upgrade</Trans>',
        expected: "Click <0>here</0> to upgrade",
      },
    ])("should extract message $case", async ({ code, expected }) => {
      const { message } = await extract(code);
      expect(message).toEqual(expected);
    });
  });

  describe("nested message formats", () => {
    it.each([
      {
        case: "plural in <Trans>",
        code: '<Trans>{$plural(count, { one: "# browser", other: "# browsers" })}</Trans>',
        expected: "{count, plural, one {# browser} other {# browsers}}",
      },
      {
        case: "plural in <Trans> among text",
        code: '<Trans>You have {$plural(count, { one: "# browser", other: "# browsers" })} open</Trans>',
        expected:
          "You have {count, plural, one {# browser} other {# browsers}} open",
      },
      {
        case: "plural in <Trans> with variables in its categories",
        code: "<Trans>{$plural(count, { one: `# browser on ${platform}`, other: `# browsers on ${platform} device(s)` })}</Trans>",
        expected:
          "{count, plural, one {# browser on {platform}} other {# browsers on {platform} device(s)}}",
      },
      {
        case: "plural inside a tag in <Trans>",
        code: '<Trans>Click <strong>{$plural(count, { one: "# time", other: "# times" })}</strong> now</Trans>',
        expected:
          "Click <0>{count, plural, one {# time} other {# times}}</0> now",
      },
      {
        case: "plural as a $t descriptor message",
        code: '{$t({ message: $plural(count, { one: "# browser", other: "# browsers" }) })}',
        expected: "{count, plural, one {# browser} other {# browsers}}",
      },
      {
        case: "$t in a plural category",
        code: "{$plural(n, { one: $t`One ${genre} book`, other: $t`# ${genre} books` })}",
        expected: "{n, plural, one {One {genre} book} other {# {genre} books}}",
      },
      {
        case: "plural in a plural category",
        code: '{$plural(n, { one: "one", other: $plural(m, { one: "# inner", other: "# inners" }) })}',
        expected:
          "{n, plural, one {one} other {{m, plural, one {# inner} other {# inners}}}}",
      },
      {
        case: "plural in a $t template literal",
        code: '{$t`You have ${$plural(count, { one: "# browser", other: "# browsers" })} open`}',
        expected:
          "You have {count, plural, one {# browser} other {# browsers}} open",
      },
    ])("should extract message with $case", async ({ code, expected }) => {
      const { message } = await extract(code);
      expect(message).toEqual(expected);
    });

    it.each([
      {
        case: "<Trans> attribute",
        code: '<Trans context="Browsers group by platform">{$plural(count, { one: "# browser", other: "# browsers" })}</Trans>',
      },
      {
        case: "$t descriptor",
        code: '{$t({ message: $plural(count, { one: "# browser", other: "# browsers" }), context: "Browsers group by platform" })}',
      },
    ])("should extract context from the $case", async ({ code }) => {
      const { context } = await extract(code);
      expect(context).toEqual("Browsers group by platform");
    });

    // The nested format is part of the enclosing message, so it must not also
    // be extracted on its own — that would put an unused entry in the
    // catalogue for every nested plural.
    it.each([
      {
        case: "<Trans>",
        code: '<Trans>{$plural(count, { one: "# browser", other: "# browsers" })}</Trans>',
      },
      {
        case: "a $t descriptor",
        code: '{$t({ message: $plural(count, { one: "# browser", other: "# browsers" }) })}',
      },
      {
        case: "a plural category",
        code: "{$plural(n, { one: $t`One book`, other: $t`# books` })}",
      },
    ])("should extract one message for a plural in $case", async ({ code }) => {
      const messages = await extractAll(code);
      expect(messages).toHaveLength(1);
    });

    it("should keep positional keys distinct across nesting", async () => {
      const { message } = await extract(
        '<Trans>{"a"} and {$plural(n, { one: `${"b"} x`, other: `${"c"} y` })}</Trans>',
      );
      expect(message).toEqual("{0} and {n, plural, one {{1} x} other {{1} y}}");
    });

    it.each([
      {
        case: "a string",
        code: '<Trans>Hello {"John"}</Trans>',
      },
      {
        case: "a template literal",
        code: "<Trans>Hello {`Mr ${name}`}</Trans>",
      },
    ])(
      "should leave a placeholder for an expression holding $case",
      async ({ code }) => {
        const { message } = await extract(code);
        expect(message).toEqual("Hello {0}");
      },
    );
  });

  // Each finder is handed only the names it matches. A component is a message
  // only when it is named `Trans`, whatever the message functions are called,
  // so widening one cannot quietly widen another.
  describe("tag matching", () => {
    it.each([
      { case: "<Plural>", code: "<Plural>Hello world</Plural>" },
      { case: "<T>", code: "<T>Hello world</T>" },
      { case: "<Translate>", code: "<Translate>Hello world</Translate>" },
      { case: "<Message>", code: "<Message>Hello world</Message>" },
    ])("should extract nothing from $case", async ({ code }) => {
      expect(await extractAll(code)).toEqual([]);
    });

    it.each([
      { case: "a tagged template", code: "{t`Hello world`}" },
      { case: "a call expression", code: '{t({ message: "Hello world" })}' },
      {
        case: "a plural",
        code: '{plural(1, { one: "One", other: "# many" })}',
      },
    ])(
      "should extract nothing from $case without the $ prefix",
      async ({ code }) => {
        expect(await extractAll(code)).toEqual([]);
      },
    );
  });
});
