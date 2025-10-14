import { svelteExtractor } from "./extractor";
import { describe, expect } from "vitest";
import { ExtractedMessage } from "@lingui/conf";

const FILE_NAME = "test.svelte";

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
});
